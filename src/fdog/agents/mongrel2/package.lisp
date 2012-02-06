(defpackage #:mongrel2-agent
  (:documentation "Mongrel2 afdog agent and compontents.")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5
        #:fdog-agent)
  (:import-from :arnesi
                :it
                :curry
                :rcurry
                :awhen)
  (:import-from :alexandria :flatten)
  (:import-from :fdog-models
                :with-clsql-retry)
  (:export :mongrel2-agent
           :*control-port*
           :ensure-mongrel2-root-layout
           :initialize-mongrel2-configuration))

(in-package :mongrel2-agent)

(defcategory mongrel2-agent)
