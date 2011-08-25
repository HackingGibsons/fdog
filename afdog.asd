(asdf:defsystem #:afdog
  :depends-on (#:uuid
               #:log5
               #:bordeaux-threads
               #:zeromq)
  :components ((:module "src" :components
                        ((:file "package" :depends-on ("patches"))
                         (:file "logging" :depends-on ("package"))

                         (:module "patches" :components
                                  ((:file "zmq")))

                         (:module "agent"
                                  :depends-on ("logging")
                                  :components
                                  ((:file "package")))))))

