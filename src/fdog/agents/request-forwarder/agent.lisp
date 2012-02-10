(in-package :request-forwarder-agent)

;; Agent
(defcategory request-forwarder-agent)
(defclass request-forwarder-agent (request-processing-agent standard-leaf-agent)
  ((forwarder :initform "x-NO-forwarder"
              :initarg :forwarder
              :accessor forwarder)
   (route :initform "x-NO-route"
          :initarg :route
          :accessor route)
   (path :initform ""
         :accessor path))
  (:default-initargs . (:handle "forwarder-x-undefined"))
  (:documentation "This agent attempts to forward requests from
external clients to internal services."))

(defmethod initialize-instance :after ((agent request-forwarder-agent) &key)
  "Bind a `handler-name' to the agent based on the `forwarder' and `route'"
  (setf (handler-name agent)
        (format nil "forwarder-~A-~A" (forwarder agent) (route agent))))

;; Agent Hooks
(defmethod agent-provides :around ((agent request-forwarder-agent))
  "Provide forwarding information."
  (append (call-next-method)
          `(:forwarding (:forwarder ,(forwarder agent)
                         :route ,(route agent)
                         :path ,(path agent)))))

