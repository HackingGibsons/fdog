(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "suites" :depends-on ("package"))
                             (:file "fixtures" :depends-on ("helpers"))

                             (:module "helpers" :depends-on ("package")
                              :components ((:file "utils")))

                             (:module "tests" :depends-on ("suites")
                              :components ((:file "fdog-models")))))))

