(in-package :afdog-tests)

;; Test structures
(defclass test-agent (agent::standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

;; Test fixtures
(def-fixtures agent-and-runner ()
  (agent (make-instance 'test-agent))
  (agent-runner (lambda () (agent::run-agent agent))))

;; Test cases
(def-test (can-test-nothing :group basic-tests) :true
  t)

(def-test (fresh-agent-is-fresh :group basic-tests :fixtures (agent-and-runner))
    (:all (:apply agent::agent-event-count (:predicate zerop))
          (:apply agent::agent-context (:not :true)))
  agent)

