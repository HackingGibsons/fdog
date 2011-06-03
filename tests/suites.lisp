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

(def-test-group database-basic-tests (database/connected)
  (:documentation "'Database baisc tests")

  (def-test test-database-sanity :pass)

  (def-eval-test test-database-insanity
      (assert-null nil)))

(def-test-group main-tests ()
  (:documentation "'The' test group.")
  (:include-groups database-basic-tests))

