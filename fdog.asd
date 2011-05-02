;;;; fdog.asd

(asdf:defsystem #:fdog
  :serial t
  :depends-on (#:clsql
               #:external-program
               #:log5
               #:uuid
               #:cl-ppcre
               #:uffi)
  :components ((:file "package")
               (:file "models/package")
               (:file "m2sh/package")

               ;; Main package
               (:file "fdog")

               ;; Models package
               (:file "models/data")
               (:file "models/helpers")
               (:file "models/config")
               (:file "models/methods")))

(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:fiveam)
  :components ((:file "tests/package")
               (:file "tests/fdog-models")

               ;; runner has to come after all the test suites are loaded 
               (:file "tests/runner")))


