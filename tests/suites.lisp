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
