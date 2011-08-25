(asdf:defsystem #:afdog
  :depends-on (#:uuid
               #:log5
               #:bordeaux-threads
               #:zeromq)
  :components ((:module "src"
                        :components ((:file "package")))))

