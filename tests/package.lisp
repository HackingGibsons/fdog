(defpackage #:fdog-tests
  (:use #:cl
        #:5am
        #:fdog
        #:fdog-models)
  (:shadowing-import-from :log5
                          :log-for))
(in-package :fdog-tests)

(defvar +server-name+ "testing")
(defvar +server-bind+ "127.0.0.1")
(defvar +server-port+ 7357)
