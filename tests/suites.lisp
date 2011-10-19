(in-package :afdog-tests)

(def-test-group all-tests ()
  (:documentation "All tests are rooted here")
  (:include-groups basic-tests))

;; Directly runnable
(def-test-group basic-tests ())

;; Event ran
(def-test-group booted-agent-tests ())
(def-test-group running-with-events-tests ())
(def-test-group terminated-agent-tests ())

