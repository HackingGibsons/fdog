(in-package :fdog-tests)

;; ;; Grouping master-suites
;; (def-suite main :description "'The' test suite for fdog")
;; (def-suite mongrel2 :in main
;;            :description "A grouping of mongrel2 tests")


;; ;; Actual test-grouping suites
;; (def-suite basic :in main
;;            :description "Basic tests, bordering on sanity checks")

;; (def-suite mongrel2/db :in mongrel2
;;            :description "Basic tests that involve Mongrel2")

;; (def-suite mongrel2/proc :in mongrel2
;;            :description "Tests that involve a running Mongrel2")


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

