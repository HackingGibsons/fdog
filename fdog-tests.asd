(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:fiveam)
  :components ((:file "tests/package")
               (:file "tests/fdog-models")

               ;; runner has to come after all the test suites are loaded
               (:file "tests/runner")))
