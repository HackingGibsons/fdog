(defpackage #:afdog
  (:use #:cl)
  (:use #:log5)
  (:export :start-logging
           :stop-logging
           :make-local-sock
           :get-local-address))

(in-package :afdog)
