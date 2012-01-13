(in-package :afdog-tests)

;; Test runner for a core with everything already loaded
(defmethod make-runner ((style (eql :test)) &rest keys &key)
  (remf keys :init)
  (remf keys :include)
  (change-class (apply #'make-runner :exec keys) 'test-runner))

(defmethod update-instance-for-different-class :after ((old exec-runner) (new test-runner) &key)
  (setf (init-forms new) nil))

(in-package :afdog)
;; Enable output traces of running programs
(setf *create-output-logs* t)
