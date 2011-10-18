(in-package :afdog-tests)

;; Test structures
(defclass test-agent (agent::standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

;; Test fixtures
(def-fixtures with-agent-and-runner ()
  (agent (make-instance 'test-agent))
  (agent-runner (lambda () (run-agent agent))))

;; TODO: Garbage
(def-test (can-test-nothing :group basic-tests)
    :true
  t)

