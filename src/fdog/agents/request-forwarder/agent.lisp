(in-package :request-forwarder-agent)

;; Agent
(defcategory request-forwarder-agent)
(defclass request-forwarder-agent (request-processing-agent standard-leaf-agent)
  ()
  (:default-initargs . (:handle "forwarder-x-undefined"))
  (:documentation "This agent attempts to forward requests from
external clients to internal services."))


