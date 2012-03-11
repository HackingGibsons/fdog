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

  (:export :*agent-host*
           :host-runner
           :has-agent-p
           :agent-host
           :add-agent
           :remove-agent
           :running-p
           :run-once
           :run))
(in-package :agent-host)
(defcategory agent-host)

(define-condition agent-host-error (error) ())

(defvar *agent-host* nil
  "The instance of the current `agent-host' that will
be used by any `host-runner' uses. If it does not exist
it will be created the first time a runner is used.")






