;; -*- mode: Lisp;  -*-
(asdf:defsystem #:fdog
  :depends-on (#:external-program
               #:log5
               #:uuid
               #:cl-ppcre
               #:m2cl
               #:bordeaux-threads
               #:clsql
               #:uffi)
  :in-order-to ((test-op (load-op fdog-tests)))
  :components ((:module "src"
                :components ((:file "package")
                             (:file "fdog" :depends-on ("models"))

                             (:module "models" :depends-on ("package")
                              :components ((:file "package")
                                           (:file "data"    :depends-on ("package"))
                                           (:file "helpers" :depends-on ("package"))
                                           (:file "config"  :depends-on ("helpers"))
                                           (:file "methods" :depends-on ("config"))))

                             (:module "m2sh" :depends-on ("models")
                              :components ((:file "package")))

                             (:module "control" :depends-on ("m2sh")
                              :components  ((:file "package")))))))


;; Test operation
(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :fdog))))
  (funcall (intern "RUN!" :5am)
           (intern "MAIN" :fdog-tests)))
