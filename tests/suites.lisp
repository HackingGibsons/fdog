(in-package :fdog-tests)

(def-test-group database-basic-tests ()
  (:documentation "Database baisc tests"))

(def-test-group mongrel2-database-tests ()
  (:documentation "Tests of the mongrel2 database access"))

(def-test-group mongrel2-process-tests ()
  (:documentation "Tests including a running mongrel2."))

(def-test-group fdog-forwarder-api-tests (database/connected database/inited fdog/engaged)
  (:documentation "Tests the forwarder creation and endpoint fetching APIs."))

(def-test-group fdog-forwarder-functional-tests ()
  (:documentation "Tests the forwarder creation and endpoint fetching API externally."))

;; Main suites
(def-test-group unit-tests ()
  (:documentation "'The' test group.")
  (:include-groups database-basic-tests
                   mongrel2-database-tests
                   mongrel2-process-tests
                   fdog-forwarder-api-tests))

(def-test-group functional-tests ()
  (:documentation "'The' test group.")
  (:include-groups fdog-forwarder-functional-tests))
