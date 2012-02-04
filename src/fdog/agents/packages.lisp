(defpackage #:mongrel2-agent
  (:documentation "Mongrel2 afdog agent and compontents.")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5)
  (:import-from :fdog-models
                :with-clsql-retry)
  (:import-from :arnesi
                :it
                :curry
                :rcurry
                :awhen)
  (:import-from :alexandria :flatten)
  (:export :mongrel2-agent
           :ensure-mongrel2-root-layout
           :initialize-mongrel2-configuration))

(defpackage #:afdog-hypervisor-agent
  (:documentation "afdog hypervisor agent")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5)
  (:import-from :arnesi
                :it
                :rcurry)
  (:import-from :mongrel2-agent
                :mongrel2-agent)
  (:import-from :api-agent
                :api-agent)
  (:export :afdog-hypervisor-agent))

(in-package :mongrel2-agent)

(defcategory mongrel2-agent)
(defcategory agent-needs)

(in-package :afdog-hypervisor-agent)
(defcategory afdog-hypervisor-agent)
