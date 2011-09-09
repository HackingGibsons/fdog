(defpackage #:afdog
  (:use #:cl)
  (:use #:log5)
  (:export :start-logging
           :stop-logging
           :get-local-address))

(in-package :afdog)
