(in-package :afdog-tests)

;; Test structures
(defclass test-agent (standard-agent)
  ()
  (:documentation "A `standard-agent' derivative we can insert probes into to test things."))

;; Test cases
(def-test (can-test-nothing :group basic-tests) :true
  t)

(def-test (fresh-agent-is-fresh :group basic-tests :fixtures (agent-fixture))
    (:all (:apply agent-event-count (:predicate zerop))
          (:apply agent-context (:not :true)))
  agent)

;; Drivers for the internal tests
(defmethod agent-special-event :after ((agent test-agent) (head (eql :boot)) event)
  "Boot event for the test agent, runs the `booted-agent-tests' group"
  (format t "Agent is booting.~%")
  (nst:nst-cmd :run-group booted-agent-tests))

(defmethod next-event :after ((agent test-agent))
  (format t "Events now: ~A~%" (agent::agent-event-count agent))
  (case (agent::agent-event-count agent)
    (5 (nst:nst-cmd :run-group running-with-events-tests))
    (10 (suicide agent))))

;; This test will scaffold a running agent and run any tests driven by the event loop
;; then execute the terminated agent group
(def-test (test-running-agent :group basic-tests :fixtures (agent-fixture))
    (:process (:eval (handler-case (bt:with-timeout (5)
                                     (run-agent agent))
                       (bt:timeout ()
                         (format t "Timing out!~%")
                         nil)))
              (:eval (nst:nst-cmd :run-group terminated-agent-tests))))

;; Booted agent tests
(def-test (running-agent-sanity-check :group booted-agent-tests) :true
  agent)

(def-test (running-agent-has-context :group booted-agent-tests) :true
  (agent-context agent))

(def-test (organs-have-sockets :group booted-agent-tests)
    (:each (:all (:apply agent::organ-incoming-sock :true)
                 (:apply agent::organ-outgoing-sock :true)))
  (agent-organs agent))


;; Termianted agent tests
(def-test (organs-closed-sockets :group terminated-agent-tests)
    (:each (:all (:apply agent::organ-incoming-sock (:not :true))
                 (:apply agent::organ-outgoing-sock (:not :true))))
  (agent-organs agent))

;; Tests to run after some events fire
(def-test (running-agent-has-events :group running-with-events-tests)
    (:predicate (lambda (v) (> v 0)))
  (agent-event-count agent))
