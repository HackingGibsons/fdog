(in-package :afdog-tests)

;; Driver
(defmethod agent-special-event :after ((agent test-agent) (head (eql :boot)) event)
  "Boot event for the test agent, runs the `booted-agent-tests' group"
  (format t "Agent is booting.~%")
  (nst:nst-cmd :run-group booted-agent-tests))

;; Booted agent tests
(def-test (running-agent-sanity-check :group booted-agent-tests) :true
  agent)

(def-test (running-agent-has-context :group booted-agent-tests) :true
  (agent-context agent))

(def-test (organs-have-sockets :group booted-agent-tests)
    (:each (:all (:apply organ-incoming-sock :true)
                 (:apply organ-outgoing-sock :true)))
  (agent-organs agent))

;; This test will scaffold a running agent and run any tests driven by the event loop
;; then execute the terminated agent group
(def-test (test-running-agent :group basic-tests :fixtures (agent-fixture))
    (:process (:eval (handler-case (bt:with-timeout (10)
                                     (agent-host:add-agent host agent)
                                     (agent-host:run host))
                       (bt:timeout ()
                         (format t "Timing out removing and running..~%")
                         (agent-host:remove-agent host agent)
                         (agent-host:run host)
                         (format t "Done!~%")
                         nil)))
              (:eval (nst:nst-cmd :run-group terminated-agent-tests))))

;; Termianted agent tests
(def-test (organs-closed-sockets :group terminated-agent-tests)
    (:each (:all (:apply organ-incoming-sock (:not :true))
                 (:apply organ-outgoing-sock (:not :true))))
  (agent-organs agent))
