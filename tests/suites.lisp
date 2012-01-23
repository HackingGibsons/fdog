(in-package :afdog-tests)

(def-test-group all-tests ()
  (:documentation "All tests are rooted here")
  (:include-groups basic-tests
                   runner-tests
                   basic-behavior-tests
                   supervision-tests
                   cli-tests
                   mongrel2-agent-tests
                   afdog-hypervisor-agent-tests
                   request-processing-agent-tests
                   api-agent-tests
                   forwarder-agent-tests))

;; Directly runnable
(def-test-group basic-tests ())
(def-test-group cli-tests (afdog-bin-fixture kill-everything-fixture)
  (:setup (unless (probe-file afdog-bin)
            (format t "Making binary.~%")
            (afdog:run-program "/usr/bin/make" `("-C" ,(namestring afdog:*root*) "afdog") :wait t)
            (format t "Binary made.~%"))))

;; Event ran
(def-test-group booted-agent-tests ())
(def-test-group running-with-events-tests ())
(def-test-group terminated-agent-tests ())


;; Running agent required
(def-test-group runner-tests (running-agent-fixture))

(def-test-group basic-behavior-tests ())

(def-test-group supervision-tests (spawner-fixture running-hypervisor-fixture kill-everything-fixture)
                (:each-setup (with-agent-conversation (m e :timeout 20) agent-uuid
                               (zmq:send! e (prepare-message `(:reset :timeout))))))

;; Agent specific
(def-test-group mongrel2-agent-tests (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
(def-test-group afdog-hypervisor-agent-tests (kill-everything-fixture))
(def-test-group request-processing-agent-tests
    (db-path-fixture
     mongrel2-agent-fixture
     request-processing-agent-fixture
     kill-everything-fixture))
(def-test-group api-agent-tests
    (db-path-fixture
     api-agent-fixture
     kill-everything-fixture))
(def-test-group forwarder-agent-tests (kill-everything-fixture))
(def-test-group forwarder-agent-tests (db-path-fixture mongrel2-agent-fixture forwarder-agent-fixture kill-everything-fixture))
(def-test-group forwarder-agent-tests (db-path-fixture forwarder-agent-fixture kill-everything-fixture))
