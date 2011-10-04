(asdf:defsystem #:afdog
  :depends-on (#:closer-mop
               #:ip-interfaces
               #:iolib.sockets ;; TODO: Vendor this with libfixpostfix as in fdog
               #:uuid
               #:log5
               #:bordeaux-threads
               #:zeromq)
  :components ((:module "src" :components
                        ;; Base project scaffold
                        ((:file "package" :depends-on ("patches"))
                         (:file "logging" :depends-on ("package"))
                         (:file "utils" :depends-on ("package"))

                         ;; Code loaded into dependencies
                         (:module "patches" :components
                                  ((:file "zmq")))

                         ;; Agent components and standard library
                         (:module "agent" :depends-on ("logging" "utils") :components
                                  ((:file "package")
                                   (:file "agent" :depends-on ("classes" "organs" "behavior"))

                                   ;; Agent classes and bases
                                   (:module "classes" :depends-on ("package") :components
                                            ((:file "standard-agent")
                                             (:file "standard-organ")))
                                   (:module "methods" :depends-on ("classes") :components
                                            ((:module "standard-agent" :components
                                                      ((:file "standard-agent")))
                                             (:file "standard-organ")))

                                   ;; Behavior definiton
                                   (:module "behavior" :depends-on ("package") :components
                                            ((:file "classes")
                                             (:file "defbehavior" :depends-on ("classes"))
                                             (:module "behaviors" :depends-on ("defbehavior") :components
                                                      ((:file "peers")))))

                                   ;; Standard organ definitions
                                   (:module "organs" :depends-on ("classes") :components
                                            ((:module "classes" :components
                                                      ((:file "heart")
                                                       (:file "head")
                                                       (:file "mouth")
                                                       (:file "ear")
                                                       (:file "appendix")))
                                             (:module "methods" :depends-on ("classes") :components
                                                      ((:file "heart")
                                                       (:file "head")))))))))))



