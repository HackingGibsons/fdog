;;;; fdog.asd

(asdf:defsystem #:fdog
  :serial t
  :depends-on (#:external-program
               #:log5
               #:uuid
               #:cl-ppcre
               #:m2cl
               #:bordeaux-threads
               #:clsql
               #:uffi)
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
