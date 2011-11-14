(in-package :afdog-cli)

(defcommand kill (argv)
  "Kill the agents listed by UUID[s]"
  (with-cli-options (argv "Usage: kill [options] uuid .. uuid~%~@{~A~%~}~%")
      (&parameters
       (timeout "Maximum length of time to allocate to the kill task.")
       &free agent-uuids)
    (with-agent-conversation (m e :timeout (or timeout 10)) agent-uuids
      (unless (mapc #'(lambda (uuid)
                        (format t "Killing -> ~A~%" uuid)
                        (zmq:send! e (prepare-message `(:agent :kill :kill ,uuid))))
                    agent-uuids)

        (format t "[ERROR] No UUIDs were supplied.~%")
        (funcall (get-command :help :function) `("kill"))))))

(defcommand agents (argv)
  "List the agents available for spawning."
  (with-cli-options (argv "Usage: agents [options]~%~@{~A~%~}~%") nil
    (flet ((agents-in-package (package)
             (let (agents)
               (do-external-symbols (agent (find-package package) agents)
                 (when (and (find-class agent nil)
                            (find (find-class 'standard-agent)
                                  (c2mop:class-direct-superclasses (find-class agent))))
                   (push agent agents))))))
      (format t "Available agents:~%~{  ~A~%~}"
              (loop for package in *agent-packages* appending (agents-in-package package))))))

(defcommand start (argv)
  "Start an instance of the named agent with the given options."
  (with-cli-options (argv "Usage: start [options] agent-type~%~@{~A~%~}~%")
      (&parameters (parent-uuid "The uuid of the parent agent to declare")
                   (parent-mouth "The mouth address of the parent named by the uuid. If omitted, a local IPC sock will be attempted instead.")
       &free agent-names)
    (when (and parent-uuid (not parent-mouth))
      (format t "WARNING: UUID specified, but not an address. Trying local IPC.~%")
      (setf parent-mouth (local-ipc-addr parent-uuid :mouth)))

    (labels ((find-symbol-in-agent-packages (symbol)
               "Look up a symbol and quickly test if it's a class. Else, nil"
               (car (mapcar #'(lambda (package)
                                (let ((sym (find-symbol (symbol-name symbol) package)))
                                  (and (find-class sym nil)
                                       sym)))
                            *agent-packages*)))

             (find-agent-or-explode (agent)
               "Find an agent by the given name or raise an error"
               (let* ((agent-sym (ignore-errors (read-from-string agent)))
                      (agent-sym (and agent-sym (symbolp agent-sym)
                                      (find-symbol-in-agent-packages agent-sym))))
                 (or agent-sym
                     (error "No such agent `~A'" agent))))

             (start-agent (agent)
               "Start the agent named by the symbol `agent'"
               (let* ((uuid (format nil "~A" (uuid:make-v4-uuid)))
                      (runner (eval `(make-runner ,*agent-spawner* :class ',agent :uuid ,uuid
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
  (when exit (quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))
