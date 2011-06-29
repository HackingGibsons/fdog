;; -*- mode: Lisp;  -*-
(asdf:defsystem #:fdog
  :depends-on (#:ip-interfaces
               #:cl-fad
               #:iolib.sockets
               #:drakma
               #:cl-who
               #:external-program
               #:unix-options
               #:log5
               #:uuid
               #:cl-ppcre
               #:m2cl
               #:bordeaux-threads
               #:clsql
               #:uffi
               #:cffi)
  :in-order-to ((test-op (load-op fdog-tests)))
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "package" :depends-on ("utils"))
                             (:file "conf" :depends-on ("package"))
                             (:file "logging" :depends-on ("package" "conf" "utils"))
                             (:file "fdog" :depends-on ("conf" "models" "control" "services" "logging"))

                             (:module "models" :depends-on ("package")
                              :components ((:file "package")
                                           (:file "data"    :depends-on ("package"))
                                           (:file "helpers" :depends-on ("package"))
                                           (:file "config"  :depends-on ("helpers"))
                                           (:file "methods" :depends-on ("config"))))

                             (:module "handler" :depends-on ("models")
                              :components ((:file "package")
                                           (:file "streams" :depends-on ("package"))
                                           (:file "router" :depends-on ("package"))
                                           (:file "request-handler" :depends-on ("package" "streams"))
                                           (:file "bridges" :depends-on ("request-handler"))))

                             (:module "m2sh" :depends-on ("models")
                              :components ((:file "package")))

                             (:module "control" :depends-on ("m2sh" "handler")
                              :components  ((:file "package")
                                            (:file "interface" :depends-on ("package"))

                                            (:module "app" :depends-on ("interface")
                                             :components ((:file "bootstrap")
                                                          (:file "routes" :depends-on ("bootstrap"))
                                                          (:module "controllers" :depends-on ("bootstrap")
                                                           :components ((:file "root")
                                                                        (:file "api")))))))

                             (:module "cli" :depends-on ("control" "m2sh" "models" "utils" "fdog" "package")
                              :components ((:file "package")
                                           (:file "cmd-utils" :depends-on ("package"))
                                           (:file "proc-utils" :depends-on ("package"))
                                           (:file "configuration" :depends-on ("package"))
                                           (:file "main" :depends-on ("cmd-utils"))))

                             (:module "services" :depends-on ("models" "control" "m2sh" "utils" "package")
                              :components ((:file "package" :depends-on ("forwarder"))
                                           (:module "forwarder"
                                            :components ((:file "package")
                                                         (:file "utils" :depends-on ("package"))
                                                         (:file "forwarder" :depends-on ("models" "classes" "methods" "ensure-ance"))

                                                         (:file "models" :depends-on ("utils"))
                                                         (:file "ensure-ance" :depends-on ("package" "models" "utils"))

                                                         (:module "classes" :depends-on ("package" "models")
                                                          :components ((:file "engine")
                                                                       (:file "multibridge" :depends-on ("engine"))
                                                                       (:file "endpoint" :depends-on ("engine" "multibridge"))))

                                                         (:module "methods" :depends-on ("models" "utils" "ensure-ance" "classes")
                                                          :components ((:file "api")
                                                                       (:file "setup")
                                                                       (:file "engine")))))))))))



;; Test operation
(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :fdog))))
  (funcall (intern (symbol-name :start-logging) :fdog))
  (funcall (intern (symbol-name :run) :fdog-tests)))
