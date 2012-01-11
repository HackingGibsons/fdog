(in-package :request-processing-agent)

;; Agent
(defclass request-processing-agent (standard-agent)
  ((handler-name :initform "x-noop"
                 :initarg :handle
                 :initarg :handler-name
                 :accessor handler-name))
  (:documentation "This agent should seek out handlers with the name `handler-name' and connect
them into it's request processing operations."))



