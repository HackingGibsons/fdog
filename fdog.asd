;;;; fdog.asd

(asdf:defsystem #:fdog
  :serial t
  :depends-on (#:clsql)
  :components ((:file "package")
               (:file "fdog")
               (:file "models/package")
               (:file "models/config")))

