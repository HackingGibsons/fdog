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
  :components ((:file "package")
               (:file "models/package")
               (:file "m2sh/package")
               (:file "control/package")

               ;; Main package
               (:file "fdog")

               ;; Models package
               (:file "models/data")
               (:file "models/helpers")
               (:file "models/config")
               (:file "models/methods")))
