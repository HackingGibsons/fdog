(in-package :afdog-tests)

(def-test-group basic-tests ())

(def-test-group all-tests ()
  (:documentation "All tests are rooted here")
  (:include-groups basic-tests))