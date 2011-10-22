(in-package :afdog-tests)

(def-test-group all-tests ()
  (:documentation "All tests are rooted here")
  (:include-groups basic-tests
                   runner-tests
                   basic-behavior-tests))


;; Directly runnable
(def-test-group basic-tests ())

;; Event ran
(def-test-group booted-agent-tests ())
(def-test-group running-with-events-tests ())
(def-test-group terminated-agent-tests ())


;; Running agent required
(def-test-group runner-tests (running-agent-fixture))

(def-test-group basic-behavior-tests ())

