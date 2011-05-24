(in-package :fdog-cli)

;; Commands
(defcommand help (&optional argv)
  (format t "Help~%"))

;; Commands and entries
(defun fdog-main (argv)
  (destructuring-bind (self &rest args) argv
    (format t "Main method: ~A :: ~A~%" self args)
    (format t "Commands: ~A~%" *commands*)
    (unless args
      (funcall (get-command :help :function)))))
