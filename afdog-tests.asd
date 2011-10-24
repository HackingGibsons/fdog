(defsystem #:afdog-tests
  :depends-on (#:afdog #:nst)
  :components ((:module "tests" :components
                        ((:file "package")
                         (:file "fixtures" :depends-on ("package"))
                         (:file "suites"   :depends-on ("fixtures"))

                         (:module "tests" :depends-on ("suites") :components
                                  ((:file "agent")))))))



