(in-package :request-processing-agent)

;; Agent
(defclass request-processing-agent (standard-agent)
  ((handler-name :initform "x-noop"
                 :initarg :handle
                 :initarg :handler-name
                 :accessor handler-name))
  (:documentation "This agent should seek out handlers with the name `handler-name' and connect
them into it's request processing operations."))

(defmethod initialize-instance-organs :after ((agent request-processing-agent))
  "Connect the organs specific to the `request-processing-agent`"
  (agent-connect agent (make-instance 'agent-requesticle :agent agent)))

(defmethod agent-provides :around ((agent request-processing-agent))
  "Advertise the handler we're ready to process requests for."
  (append (call-next-method)
          `(:request-processing ,(handler-name agent))))
