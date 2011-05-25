(in-package :fdog-cli)

(defparameter *self* "fdog"
  "The name of the binary.")

;; Commands
(defcommand init (argv)
  "Initialize an installation given by a path."
  (with-cli-options (argv "Usage: init [path]~%~@{~A~%~}~%")
      (&free path)
    (let* ((path (or (car path)
                     (getcwd)))
           (path (if (ppcre:scan "/$" path) path (format nil "~A/" path))))
      (format t "Initializing in: ~A~%" path)
      (setf path (ensure-directories-exist path :verbose t))

      (let ((dirs '(("server" ("logs" "run" "tmp"))
                    "run")))
        (format t "Need to create: ~A~%" dirs)))))

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

;; Commands and entries
(defun fdog-main (argv)
  ;; Don't want a CLI app dropping to a debugger repl
  (sb-ext:disable-debugger)

  (destructuring-bind (self &rest args) argv
    (setf *self* self)

    (unless args
      (funcall (get-command :help :function) nil))

    (let ((cmd (get-command (first args))))
      (if (not cmd)
          (progn
            (format t "~A is not a valid command.~%" (first args))
            (funcall (get-command :help :function) nil))
          (funcall (cdr cmd) (rest args))))))
