(in-package :fdog-tests)

(def-test-group database-basic-tests ()
  (:documentation "Database baisc tests"))

(def-test-group mongrel2-database-tests ()
  (:documentation "Tests of the mongrel2 database access"))

(def-test-group mongrel2-process-tests ()
  (:documentation "Tests including a running mongrel2."))

(def-test-group main-tests ()
  (:documentation "'The' test group.")
  (:include-groups database-basic-tests
                   mongrel2-database-tests
                   mongrel2-process-tests))

