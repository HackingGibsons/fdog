;;;; fdog.asd

(asdf:defsystem #:fdog
  :serial t
  :depends-on (#:clsql)
  :components ((:file "package")
               (:file "models/package")
               (:file "fdog")
               (:file "models/config")))

