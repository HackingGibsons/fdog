(in-package :afdog-tests)

(def-test-group basic-tests ())

(def-test-group booted-agent-tests ()
  (:cleanup (let ((head (agent::find-organ agent :head)))
              (agent::suicide head))))

(def-test-group all-tests ()
  (:documentation "All tests are rooted here")
  (:include-groups basic-tests))
