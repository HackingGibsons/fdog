(in-package :fdog-tests)

(def-suite main :description "'The' test suite for fdog")

(def-suite basic :in main
           :description "Basic tests, bordering on sanity checks")

(def-suite mongrel2/db :in main
           :description "Basic tests that involve Mongrel2")
