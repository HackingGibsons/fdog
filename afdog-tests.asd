(defsystem #:afdog-tests
  :depends-on (#:afdog
               #:nst)
  :components ((:module "tests" :components
                        ((:file "package")
                         (:file "helpers" :depends-on ("package"))

                         (:module "fixtures" :depends-on ("package") :components
                                  ((:module "behaviors" :components
                                            ((:file "triggers")))

                                   (:file "agents" :depends-on ("behaviors"))

                                   (:file "fixtures" :depends-on ("agents"))))

                         (:file "suites"   :depends-on ("fixtures"))

                         (:module "tests" :depends-on ("suites" "helpers") :components
                                  ((:file "basic")
                                   (:file "mongrel2")
                                   (:file "afdog-hypervisor")
                                   (:file "request-processing")
                                   (:file "booted")
                                   (:file "cli")
                                   (:file "behavior")
                                   (:file "supervision")
                                   (:file "agent")))))))
