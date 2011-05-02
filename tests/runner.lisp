(in-package :fdog-tests)

(defparameter *tests-to-run* '(fdog-models-tests))

(defun run-fdog-tests ()
  (mapc #'run! *tests-to-run*))

(run-fdog-tests)