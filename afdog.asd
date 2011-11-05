(asdf:defsystem #:afdog
  :depends-on (#:closer-mop
               #:ip-interfaces
               #:iolib.sockets ;; TODO: Vendor this with libfixpostfix as in fdog
               #:uuid
               #:swank
               #:unix-options
               #:log5
               #:bordeaux-threads
               #:cl-ppcre
               #:zeromq
               #:trivial-gray-streams)
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
                                                                ((:file "watch-machine")))
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

                         (:module "cli" :depends-on ("agent") :components
                                  ((:file "package")
                                   (:file "defcommand" :depends-on ("package"))
                                   (:file "commands" :depends-on ("defcommand"))))))

               (:module "utils" :depends-on ("src") :components
                        ((:file "package")
                         (:file "listener")))))



;; Test operation
(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :afdog))))
  (funcall (intern (symbol-name :run-all) :afdog-tests)))
