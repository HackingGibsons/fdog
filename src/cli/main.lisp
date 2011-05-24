(in-package :fdog-cli)

(defun fdog-main (argv)
  (destructuring-bind (self &rest args) argv
    (format t "Main method: ~A :: ~A~%" self args)))
