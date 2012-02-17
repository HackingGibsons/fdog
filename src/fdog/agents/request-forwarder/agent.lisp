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
         :accessor path)

   (transforms :initform (list)
               :initarg :transforms
               :accessor transforms
               :documentation "A list of symbols, :keywords
or funcallable objects representing non-destructive transformations
of the request object in sequence."))

  (:default-initargs . (:handle "forwarder-x-undefined"))

  (:documentation "This agent attempts to forward requests from
external clients to internal services."))

(defmethod initialize-instance :after ((agent request-forwarder-agent) &key)
  "Bind a `handler-name' to the agent based on the `forwarder' and `route'"
  (setf (handler-name agent)
        (format nil "forwarder-~A-~A" (forwarder agent) (route agent))))

(defmethod initialize-instance-organs :after ((agent request-forwarder-agent))
  "Connect the organs specific to the `agent'"
  (agent-connect agent (make-instance 'agent-sock-pocket :agent agent)))

;; Agent Hooks
(defmethod agent-special-event :after ((agent request-forwarder-agent) (event-head (eql :boot)) event)
  ;; Disable the requesticle on boot
  ;; wait until we figure out our path rewriting rules before enabling it.
  (send-message (find-organ agent :head) :command
                `(:command :requesticle
                  :requesticle :disable)))

(defmethod agent-provides :around ((agent request-forwarder-agent))
  "Provide forwarding information."
  (let ((endpoints (list)))
    (maphash #'(lambda (name endpoint)
                 (appendf endpoints (list name
                                          (list :push (addr-of (push-sock endpoint))
                                                :sub (addr-of (sub-sock endpoint))))))
             (client-socks (find-organ agent :sock-pocket)))

    (append (call-next-method)
            `(:forwarding (:forwarder ,(forwarder agent)
                           :route ,(route agent)
                           :path ,(path agent)
                           :endpoints ,endpoints)))))

(defmethod heard-message :after ((agent request-forwarder-agent) (organ agent-head) (from (eql :agent)) (type (eql :info)) &rest info)
  (when-bind forwarder (assoc (forwarder agent)
                              (getf (getf (getf info :info) :provides) :forwarders) :test #'string=)
    (when-bind routes (getf (rest forwarder) :routes)
      (when-bind my-path (cdr (assoc (route agent) routes :test #'string=))
        ;; If we aren't using the path we hear, set the new one
        ;; and ask the requesticle to start processing.
        (unless (string= (path agent) my-path)
          (log-for (request-forwarder-agent trace) "My path is now: ~S" my-path)
          (setf (path agent) my-path)
          (send-message organ :command
                `(:command :requesticle
                  :requesticle :enable)))))))
