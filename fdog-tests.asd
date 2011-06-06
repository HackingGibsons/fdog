;; -*- mode: Lisp;  -*-
(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:nst
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "suites" :depends-on ("package"))
;                             (:file "fixtures" :depends-on ("helpers"))

;                             (:module "helpers" :depends-on ("package")
;                              :components ((:file "utils")))

                             (:module "tests" :depends-on ("suites")
                              :components (;(:file "main")
                                           ;(:file "mongrel2.db")
                                           ;(:file "mongrel2.proc" :depends-on ("mongrel2.db"))
                                           (:file "basic")))))))

