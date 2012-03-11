(defpackage #:agent-host
  (:use :cl)
  (:use :log5)
  (:use :afdog)
  (:use :agent)

  (:import-from :arnesi
                :it
                :curry
                :if-bind
                :when-bind
                :awhen)
  (:import-from :alexandria
                :appendf
                :flatten)

  (:export :agent-host
           :add-agent
           :remove-agent
           :run-once
           :run))
(in-package :agent-host)

;; Basic logging category for the package
(defcategory agent-host)

;; Basic condition type for the package
(define-condition agent-host-error (error) ())






