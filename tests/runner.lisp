(in-package :fdog-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *tests-to-run* '(fdog-models-tests))
  
  (defun run-fdog-tests ()
    (mapc #'run! *tests-to-run*))

  (run-fdog-tests))