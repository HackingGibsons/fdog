(in-package :fdog-tests)

;; Grouping master-suites
(def-suite main :description "'The' test suite for fdog")
(def-suite mongrel2 :in main
           :description "A grouping of mongrel2 tests")


;; Actual test-grouping suites
(def-suite basic :in main
           :description "Basic tests, bordering on sanity checks")

(def-suite mongrel2/db :in mongrel2
           :description "Basic tests that involve Mongrel2")

(def-suite mongrel2/proc :in mongrel2
           :description "Tests that involve a running Mongrel2")


;; NST
(def-fixtures database/connected
    (:setup (log-for (trace) "DB Connected setup")
     :cleanup (log-for (trace) "DB Disconnected"))
  (db-path "something"))

(def-fixtures database/configured
    (:setup (log-for (trace) "DB configured setup")
     :cleanup (log-for (trace) "DB configured cleanup"))
  (db-init "something-else"))

(def-fixtures mongrel2/running
    (:setup (log-for (trace) "Mongrel2 setup")
     :cleanup (log-for (trace) "Mongrel2 cleanup"))
  (mongrel2 "something-else-entirely"))



(def-test-group database-basic-tests (database/connected)
  (:documentation "Database baisc tests"))

(def-test-group mongrel2-database-tests (database/connected database/configured)
  (:documentation "Tests of the mongrel2 database access"))

(def-test-group mongrel2-process-tests (database/connected database/configured mongrel2/running)
  (:documentation "Tests including a running mongrel2."))

(def-test-group main-tests ()
  (:documentation "'The' test group.")
  (:include-groups database-basic-tests
                   mongrel2-database-tests
                   mongrel2-process-tests))

