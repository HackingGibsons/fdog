(asdf:defsystem #:afdog
  :depends-on (#:uuid
               #:log5
               #:bordeaux-threads
               #:zeromq)
  :components ((:module "src" :components
                        ((:file "package")
                         (:file "logging" :depends-on ("package"))

                         (:module "agent"
                                  :depends-on ("logging")
                                  :components
                                  ((:file "package")))))))

