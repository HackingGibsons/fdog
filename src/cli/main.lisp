(in-package :fdog-cli)

;; Commands
(defcommand init (argv)
  "Initialize an installation given by a path."
  (declare (ignorable argv)))


(defcommand help (self &optional argv)
  "Show help"
  (declare (ignorable argv))

  (format t "Usage: ~A <command> [command-options]~%" self)
  (format t "Available commands:~%")
  (dolist (command *commands*)
    (let* ((cmd-name (car command))
           (cmd-doc (get-command cmd-name :doc)))
      (format t "=> ~A~%" (string-downcase (symbol-name cmd-name)))
      (when cmd-doc
        (format t "  ~A~%" cmd-doc)))))

;; Commands and entries
(defun fdog-main (argv)
  (destructuring-bind (self &rest args) argv
    (unless args
      (funcall (get-command :help :function) self))))
