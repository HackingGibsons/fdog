(in-package :afdog-cli)

(defcommand start (argv)
  "Start an instance of the named agent with the given options."
  (with-cli-options (argv "Usage: start [options] agent-type~%~@{~A~%~}~%")
      (&parameters (parent-uuid "The uuid of the parent agent to declare")
                   (parent-mouth "The mouth address of the parent named by the uuid. If omitted, a local IPC sock will be attempted instead.")
       &free agent-names)
    (format t "PWD: ~A ~%" (iolib.syscalls:getcwd))
    (format t "Self: ~A => ~A~%" *self* agent-names)
    (format t "UUID: ~A~%" parent-uuid)
    (format t "Mouth: ~A~%" parent-mouth)

    (labels ((find-symbol-in-agent-packages (symbol)
               (car (mapcar #'(lambda (package)
                                (let ((sym (find-symbol (symbol-name symbol) package)))
                                  (and (find-class sym nil)
                                       sym)))
                            *agent-packages*)))

             (find-agent-or-explode (agent)
               (let* ((agent-sym (ignore-errors (read-from-string agent)))
                      (agent-sym (and agent-sym (symbolp agent-sym)
                                      (find-symbol-in-agent-packages agent-sym))))
                 (or agent-sym
                     (error "No such agent `~A'" agent))))

             ;; TODO: Add the right initargs conditionally
             (start-agent (agent)
               (format t "~A~%"
                       `(start (make-runner :cli :class agent)))))

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
        (format t "Usage: ~A <command> [command-options]~%" *self*)
        (list-commands)))
  (when exit (quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))
