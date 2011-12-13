(asdf:defsystem #:afdog
  :version "0.0.0"
  :depends-on (#:closer-mop
               #:ip-interfaces
               #:uuid
               #:swank
               #:unix-options
               #:log5
               #:bordeaux-threads
               #:cl-ppcre
               #:trivial-gray-streams
               #:external-program
               #:cl-fad

               ;; Vendord
               #:clsql
               #:zeromq
               #:iolib.sockets

               ;; Somehow, if these aren't the last deps iolib or clsql fails to build :(
               #:alexandria
               #:uffi
               #:cffi)
  :in-order-to ((test-op (load-op afdog-tests)))
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
                                                                 (:file "watch-machine" :depends-on ("state-machine"))))
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
                                                      (:file "methods" :depends-on ("models"))
                                                      (:file "helpers" :depends-on ("package"))
                                                      (:file "m2sh" :depends-on ("models" "methods"))))
                                   (:module "agents" :depends-on ("mongrel2") :components
                                           ((:file "packages")
                                            (:file "mongrel2-agent" :depends-on ("packages"))))))

                         (:module "cli" :depends-on ("agent") :components
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
