(in-package :afdog-tests)

;;
;; Runner encapsulation
(defclass test-runner (agent::exec-runner) ())

(defmethod update-instance-for-different-class :after (previous (current test-runner) &key)
  (setf (agent::init-forms current)
        (append (agent::init-forms current)
                '((ql:quickload :afdog-tests)))))

(defmethod agent::make-runner ((style (eql :test)) &rest keys &key)
  (change-class (apply #'agent::make-runner :exec keys) 'test-runner))

;; Test fixtures
(def-fixtures agent-fixture
    (:special (agent))
  (agent (make-instance 'test-agent)))

(def-fixtures running-agent-fixture
    (:setup
     (unless (agent::running-p agent-runner)
       (agent::start agent-runner))
     :cleanup
     (if (agent::running-p agent-runner)
         (agent::stop agent-runner)))
  (agent-uuid (format nil "~A" (uuid:make-v4-uuid)))
  (agent-runner (agent::make-runner :test :class 'afdog-tests::runner-agent :include '(:afdog-tests) :uuid agent-uuid)))
