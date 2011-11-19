(in-package :afdog-tests)

(def-test-group all-tests ()
  (:documentation "All tests are rooted here")
  (:include-groups basic-tests
                   runner-tests
                   basic-behavior-tests
                   supervision-tests
                   cli-tests
                   mongrel2-agent-tests))


;; Directly runnable
(def-test-group basic-tests ())
(def-test-group cli-tests ())

;; Event ran
(def-test-group booted-agent-tests ())
(def-test-group running-with-events-tests ())
(def-test-group terminated-agent-tests ())


;; Running agent required
(def-test-group runner-tests (running-agent-fixture))

(def-test-group basic-behavior-tests ())

(def-test-group supervision-tests (spawner-fixture running-hypervisor-fixture)
                (:each-setup (with-agent-conversation (m e :timeout 20) agent-uuid
                               (zmq:send! e (prepare-message `(:reset :timeout))))))

(def-test-group mongrel2-agent-tests ())


