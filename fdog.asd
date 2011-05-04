;;;; fdog.asd

(asdf:defsystem #:fdog
  :serial t
  :depends-on (#:clsql
               #:external-program
               #:log5
               #:uuid
               #:cl-ppcre
               #:m2cl
               #:uffi)
  :components ((:file "package")
               (:file "models/package")
               (:file "m2sh/package")

               ;; Main package
               (:file "fdog")

               ;; Models package
               (:file "models/data")
               (:file "models/helpers")
               (:file "models/config")
               (:file "models/methods")))
