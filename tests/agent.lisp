(in-package :afdog-tests)

;; Test structures
(defclass test-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

(defmethod agent-special-event :after ((agent test-agent) (head (eql :boot)) event)
  "Boot event for the test agent"
  (format t "Agent is booting.~%")
  (nst:nst-cmd :run-group booted-agent-tests))

;; Test cases
(def-test (can-test-nothing :group basic-tests) :true
  t)

(def-test (fresh-agent-is-fresh :group basic-tests :fixtures (agent-fixture))
    (:all (:apply agent-event-count (:predicate zerop))
          (:apply agent-context (:not :true)))
  agent)

;; Boots up the running agent tests
(def-test (test-running-agent :group basic-tests :fixtures (agent-fixture))
    (:process (:eval (handler-case (bt:with-timeout (1)
                                     (run-agent agent))
                       (bt:timeout () nil)))))

;; Running agent tests
(def-test (running-agent-sanity-check :group booted-agent-tests) :true
  agent)

(def-test (running-agent-has-context :group booted-agent-tests) :true
  (agent-context agent))
