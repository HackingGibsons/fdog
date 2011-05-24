(in-package :fdog-cli)

;; Commands
(defcommand init (argv)
  "Initialize an installation given by a path."
  (declare (ignorable argv)))


(defcommand help (self &key (exit 0) command)
  "Show help"
  (format t "Usage: ~A <command> [command-options]~%" self)
  (format t "Available commands:~%")
  (dolist (command *commands*)
    (let* ((cmd-name (car command))
           (cmd-doc (get-command cmd-name :doc)))
      (format t "=> ~A~%" (string-downcase (symbol-name cmd-name)))
      (when cmd-doc
        (format t "  ~A~%" cmd-doc))))

  (when exit (quit :unix-status 0)))

;; Commands and entries
(defun fdog-main (argv)
  ;; Don't want a CLI app dropping to a debugger repl
  (sb-ext:disable-debugger)

  (destructuring-bind (self &rest args) argv
    (unless args
      (funcall (get-command :help :function) self))
    (unless (get-command (first args))
      (format t "~A is not a valid command.~%" (first args))
      (funcall (get-command :help :function) self))))
