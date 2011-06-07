;; -*- mode: Lisp;  -*-
(asdf:defsystem #:fdog-tests
  :serial t
  :depends-on (#:fdog
               #:nst)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "fixtures" :depends-on ("helpers"))
                             (:file "suites" :depends-on ("fixtures"))

                            (:module "helpers" :depends-on ("package")
                             :components ((:file "utils")))

                             (:module "tests" :depends-on ("suites" "fixtures")
                              :components ((:file "mongrel2.db")
                                           ;(:file "mongrel2.proc" :depends-on ("mongrel2.db"))
                                           (:file "basic")))))))

