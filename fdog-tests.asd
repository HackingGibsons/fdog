(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "suites" :depends-on ("package"))

                             (:module "tests" :depends-on ("suites")
                              :components ((:file "fdog-models")))))))

