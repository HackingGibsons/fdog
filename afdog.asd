(asdf:defsystem #:afdog
  :version "0.0.0"
  :depends-on (#:closer-mop
               #:ip-interfaces
               #:uuid
               #:unix-options
               #:log5
               #:bordeaux-threads
               #:cl-ppcre
               #:trivial-gray-streams
               #:external-program
               #:cl-fad
               #:cl-syslog
               #:iolib
               #:usocket
               #:arnesi
               #:trivial-backtrace
               #:trivial-garbage
               #:drakma
               #:cl-json
               #:cl-redis

               ;; Vendord
               #:clsql
               #:zmq
               #:m2cl

               ;; Somehow, if these aren't the last deps iolib or clsql fails to build :(
               #:alexandria
               #:cffi-uffi-compat
               #:cffi)
  :in-order-to ((test-op (load-op afdog-tests)))
  :components ((:module "src" :components
                        ;; Base project scaffold
                        ((:file "package" :depends-on ("patches"))
                         (:file "logging" :depends-on ("package"))
                         (:file "utils" :depends-on ("package"))

                         ;; Code loaded into dependencies
                         (:module "patches" :components
                                  ((:file "zmq")
                                   (:file "m2cl")))

                         ;; Agent host
                         (:module "host" :depends-on ("package" "agent") :components
                                  ((:file "package")
                                   (:file "utils" :depends-on ("package"))

                                   (:file "agent-host" :depends-on ("package"))
                                   (:file "generics" :depends-on ("package"))
                                   (:file "methods" :depends-on ("package" "utils" "agent-host" "generics"))

                                   (:file "runner" :depends-on ("agent-host" "generics"))))

                         ;; Agent components and standard library
                         (:module "agent" :depends-on ("logging" "utils") :components
                                  ((:file "package")
                                   (:file "agent" :depends-on ("classes" "organs" "behavior"))
                                   (:file "helpers" :depends-on ("package" "methods"))


                                   ;; Agent classes and bases
                                   (:module "classes" :depends-on ("package") :components
                                            ((:file "standard-agent")
                                             (:file "standard-organ")))
                                   (:module "methods" :depends-on ("classes") :components
                                            ((:module "standard-agent" :components
                                                      ((:file "standard-agent")
                                                       (:file "init")))
                                             (:file "standard-organ")))

                                   ;; Behavior definiton
                                   (:module "behavior" :depends-on ("package" "organs") :components
                                            ((:file "classes")
                                             (:file "defbehavior" :depends-on ("classes"))
                                             (:module "behaviors" :depends-on ("defbehavior") :components
                                                      ((:module "helpers" :components
                                                                ((:file "state-machine")
                                                                 (:file "watch-machine" :depends-on ("state-machine"))
                                                                 (:file "process-watch-machine" :depends-on ("watch-machine"))))
                                                       (:file "peers")
                                                       (:file "manipulation")
                                                       (:file "supervision" :depends-on ("helpers"))
                                                       (:file "vision")))))

                                   ;; Standard organ definitions
                                   (:module "organs" :depends-on ("classes") :components
                                            ((:module "classes" :components
                                                      ((:file "heart")
                                                       (:file "head")
                                                       (:file "mouth")
                                                       (:file "ear")
                                                       (:file "eye")
                                                       (:file "hand")
                                                       (:file "appendix")))
                                             (:module "methods" :depends-on ("classes") :components
                                                      ((:file "heart")
                                                       (:file "eye")
                                                       (:file "head")))))))

                         (:module "fdog" :depends-on ("agent") :components
                                  ((:file "package" :depends-on ("agents"))
                                   (:module "mongrel2" :components
                                                     ((:file "package")
                                                      (:file "data")
                                                      (:file "models" :depends-on ("package" "helpers"))
                                                      (:file "methods" :depends-on ("models" "helpers"))
                                                      (:file "helpers" :depends-on ("package"))
                                                      (:file "m2sh" :depends-on ("models" "methods" "helpers"))))

                                   (:module "agents" :depends-on ("mongrel2") :components
                                           ((:file "package")
                                            (:module "mongrel2" :depends-on ("package") :components
                                                     ((:file "package")
                                                      (:file "helpers" :depends-on ("package"))
                                                      (:file "behaviors"  :depends-on ("mongrel2-agent"))
                                                      (:file "mongrel2-agent" :depends-on ("package" "helpers"))))

                                            (:module "afdog-hypervisor" :depends-on ("package" "api-agent" "mongrel2" "forwarder") :components
                                                     ((:file "package")
                                                      (:file "afdog-hypervisor-agent" :depends-on ("package"))))

                                            (:module "request-forwarder" :depends-on ("request-processing" "api-agent") :components
                                                     ((:file "package")
                                                      (:file "endpoint" :depends-on ("package"))
                                                      (:file "storage" :depends-on ("agent-behavior" "endpoint"))
                                                      (:file "sock-pocket-organ" :depends-on ("package" "endpoint"))
                                                      (:file "agent" :depends-on ("package" "sock-pocket-organ"))
                                                      (:file "agent-behavior" :depends-on ("agent"))))

                                            (:module "api-agent" :depends-on ("request-processing") :components
                                                     ((:file "package")
                                                      (:module "http" :depends-on ("package") :components
                                                               ((:file "package")
                                                                (:file "utils" :depends-on ("package"))
                                                                (:file "router" :depends-on ("package"))
                                                                (:file "streams" :depends-on ("utils"))))

                                                      (:module "app" :depends-on ("http") :components
                                                               ((:file "package")
                                                                (:file "app" :depends-on ("package"))))

                                                      (:file "agent" :depends-on ("package" "app"))))

                                            (:module "request-processing" :depends-on ("package") :components
                                                     ((:file "package")
                                                      (:file "agent" :depends-on ("package"))
                                                      (:file "requesticle-organ" :depends-on ("package"))
                                                      (:file "behaviors" :depends-on ("requesticle-organ" "agent"))))

                                            (:module "forwarder" :depends-on ("package") :components
                                                     ((:file "package")
                                                      (:file "behaviors" :depends-on ("forwarder-agent" "children"))
                                                      (:file "children" :depends-on ("forwarder-agent"))
                                                      (:file "forwarder-agent" :depends-on ("package"))))))))

                         (:module "cli" :depends-on ("agent" "fdog") :components
                                  ((:file "package")
                                   (:file "runner" :depends-on ("package"))
                                   (:file "defcommand" :depends-on ("package"))
                                   (:file "commands" :depends-on ("defcommand"))))))

               (:module "utils" :depends-on ("src") :components
                        ((:file "package")
                         (:file "listener")))))

;; Test operation
(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :afdog))))
  (funcall (intern (symbol-name :run-all) :afdog-tests)))
