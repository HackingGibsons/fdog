(in-package :afdog-tests)

;; Test structures
(defclass test-agent (agent::standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defmethod agent::agent-special-event :after ((agent test-agent) (head (eql :boot)) event)
  "Boot event for the test agent"
  (format t "Agent is booting.~%")
  (nst:nst-cmd :run-group running-agent-tests))

;; Test cases
(def-test (can-test-nothing :group basic-tests) :true
  t)

(def-test (fresh-agent-is-fresh :group basic-tests :fixtures (agent-fixture))
    (:all (:apply agent::agent-event-count (:predicate zerop))
          (:apply agent::agent-context (:not :true)))
  agent)

(def-test (test-running-agent :group basic-tests :fixtures (agent-fixture))
    (:process (:eval (handler-case (bt:with-timeout (1)
                                     (agent::run-agent agent))
                       (bt:timeout () nil)))))

(def-test (running-agent-sanity-check :group running-agent-tests) :true
  agent)

(def-test (running-agent-has-context :group running-agent-tests) :true
  (agent::agent-context agent))
