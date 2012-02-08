(in-package :request-forwarder-agent)

(defcategory request-forwarder-agent)
(defclass request-forwarder-agent (request-processing-agent standard-agent)
  ()
  (:default-initargs . (:handle "forwarder-x-undefined"))
  (:documentation "This agent attempts to forward requests from
external clients to internal services."))


