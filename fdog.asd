;;;; fdog.asd

(asdf:defsystem #:fdog
  :serial t
  :depends-on (#:clsql)
  :components ((:file "package")
               (:file "models/package")
               (:file "m2sh/package")

               ;; Main package
               (:file "fdog")

               ;; Models package
               (:file "models/helpers")
               (:file "models/config")
               (:file "models/methods")))


