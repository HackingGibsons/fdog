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
  (:import-from :forwarder-agent
                :forwarder-agent)
  (:export :afdog-hypervisor-agent))

(in-package :afdog-hypervisor-agent)

(defcategory afdog-hypervisor-agent)
