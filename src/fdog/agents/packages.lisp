(defpackage #:mongrel2-agent
  (:documentation "Mongrel2 afdog agent and compontents.")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5)
  (:import-from :arnesi :it
                :curry
                :rcurry
                :awhen)
  (:import-from :alexandria :flatten)
  (:export :mongrel2-agent
           :rooted-agent-mixin
           :ensure-mongrel2-root-layout
           :initialize-mongrel2-configuration))
(in-package :mongrel2-agent)

(defcategory mongrel2-agent)
(defcategory agent-needs)
