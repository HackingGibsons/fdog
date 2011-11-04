(in-package :afdog-cli)

(defcommand repl (argv)
  "Start a repl and nothing else"
  #+sbcl
  (sb-impl::toplevel-repl nil))


(defcommand help (argv &key (exit 0))
  "Show help"
  (if argv
      (progn
        (format t "Help on command ~A:~%" (car argv))
        (let ((doc (get-command (car argv) :doc)))
          (when doc (format t "~A~%~%" doc)))
        (funcall (get-command (car argv) :function) '("-h")))
      (progn
        (format t "Usage: ~A <command> [command-options]~%" *self*)
        (list-commands)))
  (when exit (quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))
