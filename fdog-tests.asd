(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "runner" :depends-on ("tests"))

                             (:module "tests" :depends-on ("package")
                              :components ((:file "fdog-models")))))))

