(in-package :fdog-cli)

(defparameter *self* "fdog"
  "The name of the binary.")

;; Commands
(defcommand init (argv)
  "Initialize an installation given by a path."
  (format t "Initializing ~A~%" argv))


(defcommand help (&key (exit 0) command)
  "Show help"
  (if command
      (format t "Help on command ~A:~%" command)
      (progn
        (format t "Usage: ~A <command> [command-options]~%" *self*)
        (list-commands)))
  (when exit (quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))

;; Commands and entries
(defun fdog-main (argv)
  ;; Don't want a CLI app dropping to a debugger repl
  (sb-ext:disable-debugger)

  (destructuring-bind (self &rest args) argv
    (setf *self* self)

    (unless args
      (funcall (get-command :help :function)))

    (let ((cmd (get-command (first args))))
      (if (not cmd)
          (progn
            (format t "~A is not a valid command.~%" (first args))
            (funcall (get-command :help :function)))
          (handler-case (apply (cdr cmd) (rest args))
            (program-error (c) (funcall (get-command :help :function) :command (car cmd))))))))
