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
