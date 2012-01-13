(in-package :afdog-cli)

(defcommand list (argv)
  "List running agents that can be discovered."
  (with-cli-options (argv "Usage: list [options]~%~@{~A~%~}~%")
      (&parameters
       (timeout "How long to collect data before quiting."))
    (let ((wait (or (and timeout (parse-integer timeout))
                    25))
          agents)
      (format t "Searching for agents for ~A seconds. ^C to stop...~%" wait)
      (tagbody
         (sb-sys:enable-interrupt sb-posix:sigint #'(lambda (&rest args)
                                                      (format t "~&Exiting!~%")
                                                      (go finish)))
         (discover-agents-on-host (:traverse t :timeout wait) (uuid info)
           (prog1 nil ;; Search until we hit timeout
             (unless (find uuid agents :test #'equalp)
               (format t "Agent: ~A / ~A~%" uuid (getf info :type))
               (push uuid agents))))
       finish
         (format t "Found: ~A agents.~%" (length agents))))))

(defcommand kill (argv)
  "Kill the agents listed by UUID[s]"
  (with-cli-options (argv "Usage: kill [options] uuid .. uuid~%~@{~A~%~}~%")
      ((blind "Don't search for agents, just fire messages at where they should be.")
       &parameters
       (timeout "Maximum length of time to allocate to the kill task.")
       &free agent-uuids)

    (labels ((kill-agent (ear uuid &key (verbose t))
               (when verbose
                 (format t "Killing without query -> ~A~%" uuid))
               (zmq:send! ear (prepare-message `(:agent :kill :kill ,uuid))))

             (kill-agents (agent-uuids)
               (let (found)
                 (or (with-agent-conversation (m e :timeout (or timeout 10)) agent-uuids
                       (if blind
                           (setf found (mapc #'kill-agent
                                             (make-list (length agent-uuids) :initial-element e)
                                             agent-uuids))

                           (do* ((msg (parse-message (read-message m)) (parse-message (read-message m)))
                                 (uuid (getf (getf msg :info) :uuid) (getf (getf msg :info) :uuid))
                                 (type (getf (getf msg :info) :type) (getf (getf msg :info) :type)))
                                ((= (length found) (length agent-uuids)) found)
                             (when (and (equalp (getf msg :agent) :info)
                                        (find uuid agent-uuids :test #'equalp))
                               (format t "Killing -> ~A => ~A~%" type uuid)
                               (kill-agent e uuid :verbose nil)
                               (push uuid found)))))
                     (prog1 :timeout
                       (format t "[WARN] ~A/~A killed!~%" (length found) (length agent-uuids)))))))

      (unless (and agent-uuids (kill-agents agent-uuids))
        (format t "[ERROR] No UUIDs were supplied.~%")
        (funcall (get-command :help :function) `("kill"))))))

(defcommand kill-everything (argv)
  "Kill all running agents and spawned processes with kill -9."
  (with-cli-options (argv "Usage: kill-everything [options]~%~@{~A~%~}~%")
      (&parameters (root "An alternate root directory"))
    (afdog:kill-everything :root root)))

(defcommand agents (argv)
  "List the agents available for spawning."
  (with-cli-options (argv "Usage: agents [options]~%~@{~A~%~}~%") nil
    (flet ((agents-in-package (package)
             (let (agents)
               (do-external-symbols (agent (find-package package) agents)
                 (when (and (find-class agent nil)
                            (find (find-class 'standard-agent)
                                  (c2mop:compute-class-precedence-list (find-class agent))))
                   (push agent agents))))))
      (format t "Available agents:~%~{  ~A~%~}"
              (loop for package in *agent-packages* appending (agents-in-package package))))))

(defcommand start (argv)
  "Start an instance of the named agent with the given options."
  (with-cli-options (argv "Usage: start [options] agent-type~%~@{~A~%~}~%")
      (&parameters (uuid "Force a UUID on the agent")
                   (parent-uuid "The uuid of the parent agent to declare")
                   (parent-mouth "The mouth address of the parent named by the uuid. If omitted, a local IPC sock will be attempted instead.")
       &free agent-names)
    (when (and parent-uuid (not parent-mouth))
      (format t "WARNING: UUID specified, but not an address. Trying local IPC.~%")
      (setf parent-mouth (local-ipc-addr parent-uuid :mouth)))

    (labels ((find-symbol-in-agent-packages (symbol)
               "Look up a symbol and quickly test if it's a class. Else, nil"
               (car (remove nil (mapcar #'(lambda (package)
                                       (let ((sym (find-symbol (symbol-name symbol) package)))
                                         (and (find-class sym nil)
                                              sym)))
                                   *agent-packages*) :test #'equalp)))

             (find-agent-or-explode (agent)
               "Find an agent by the given name or raise an error"
               (let* ((agent-sym (ignore-errors (read-from-string agent)))
                      (agent-sym (and agent-sym (symbolp agent-sym)
                                      (find-symbol-in-agent-packages agent-sym))))
                 (or agent-sym
                     (error "No such agent `~A'" agent))))

             (start-agent (agent)
               "Start the agent named by the symbol `agent'"
               (let* ((uuid (or uuid (format nil "~A" (uuid:make-v4-uuid))))
                      (runner (apply #'make-runner *agent-spawner*
                                     `(:class ,agent :uuid ,uuid
                                       ,@(when parent-uuid (list :parent-uuid parent-uuid))
                                       ,@(when parent-mouth (list :parent-mouth parent-mouth))))))
                 (start runner)
                 (format t "Started agent `~A'. UUID: ~A~%" agent uuid))))

      (let ((agents (mapcar #'find-agent-or-explode agent-names)))
        (unless (mapc #'start-agent agents)
          (format t "ERROR: You must specify an agent.~%~%")
          (funcall (get-command :help :function) `("start")))))))

(defcommand repl (argv)
  "Start a REPL, or if forms are provided, evaluate the forms and terminate."
  (with-cli-options (argv "Usage: repl [options] [form1 form2 form3]~%~@{~A~%~}~%")
      (&free forms)
    (flet ((read-eval (s)
             "Read a string `s' and eval it"
             (eval (read-from-string s))))

      (or (mapc #'read-eval forms)
          #+sbcl (sb-impl::toplevel-repl nil)))))

(defcommand start-logging (argv)
  "Dumps log messages to stdout."
  (with-cli-options (argv "Usage: start-logging [options]~%~@{~A~%~}~%") nil
    (afdog:start-logging-collect)))

(defcommand help (argv &key (exit 0))
  "Show help"
  (if argv
      (with-cli-options (argv "Usage: help [options] [command]~%~@{~A~%~}~%")
        nil ;; No option bindings
        (format t "Help on command ~A:~%" (car argv))
        (let ((doc (get-command (car argv) :doc))
              (fun (get-command (car argv) :function)))
          (if (and doc fun)
              (ignore-errors
                (format t "~A~%~%" doc)
                (funcall (get-command (car argv) :function) '("-h")))
              (format t "  Unknown command.~%"))))
      (progn
        (format t "Afdog ~A~%" (afdog:version-string))
        (format t "Usage: ~A <command> [command-options]~%" *self*)
        (list-commands)))
  (when exit (sb-ext:quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))
