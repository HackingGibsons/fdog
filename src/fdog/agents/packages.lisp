(defpackage #:fdog-agent
  (:documentation "Common agent functionality for fdog agents.")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5)
  (:export :agent-needs))

(in-package :fdog-agent)

(defcategory agent-needs)

(defgeneric agent-needs (agent organ need-what need-info)
  (:documentation "Called when an :agent :need message is heard by the agent for simplified dispatch.")
  (:method (agent organ need-what need-info)
    "Default method is a whiny no-op"
    (log-for (trace agent-needs) "~A/~A does not know how to fill the need for ~A using ~A"
             agent organ need-what need-info)))

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

(in-package :afdog-hypervisor-agent)
(defcategory afdog-hypervisor-agent)
