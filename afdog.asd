(asdf:defsystem #:afdog
  :depends-on (#:uuid
               #:log5
               #:bordeaux-threads
               #:zeromq)
  :components ((:module "src" :components
                        ;; Base project scaffold
                        ((:file "package" :depends-on ("patches"))
                         (:file "logging" :depends-on ("package"))

                         ;; Code loaded into dependencies
                         (:module "patches" :components
                                  ((:file "zmq")))

                         ;; Agent components and standard library
                         (:module "agent" :depends-on ("logging") :components
                                  ((:file "package")
                                   (:file "agent" :depends-on ("classes" "organs"))

                                   ;; Agent classes and bases
                                   (:module "classes" :depends-on ("package") :components
                                            ((:file "standard-agent")
                                             (:file "standard-organ")))
                                   (:module "methods" :depends-on ("classes") :components
                                            ((:module "standard-agent" :components
                                                      ((:file "standard-agent")))
                                             (:file "standard-organ")))

                                   ;; Standard organ definitions
                                   (:module "organs" :depends-on ("classes") :components
                                            ((:module "classes" :components
                                                      ((:file "heart")))
                                             (:module "methods" :depends-on ("classes") :components
                                                      ((:file "heart")))))))))))



