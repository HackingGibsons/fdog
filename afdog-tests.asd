(defsystem #:afdog-tests
  :depends-on (#:afdog #:nst)
  :components ((:module "tests" :components
                        ((:file "package")
                         (:file "suites"   :depends-on ("package"))
                         (:file "agent"    :depends-on ("suites"))))))


  