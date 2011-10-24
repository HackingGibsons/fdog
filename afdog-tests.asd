(defsystem #:afdog-tests
  :depends-on (#:afdog #:nst)
  :components ((:module "tests" :components
                        ((:file "package")
                         (:module "fixtures" :depends-on ("package") :components
                                  ((:module "behaviors" :components
                                            ((:file "triggers")))

                                   (:file "agents" :depends-on ("behaviors"))

                                   (:file "fixtures" :depends-on ("agents"))))

                         (:file "suites"   :depends-on ("fixtures"))

                         (:module "tests" :depends-on ("suites") :components
                                  ((:file "basic")
                                   (:file "booted")
                                   (:file "behavior")
                                   (:file "agent")))))))
