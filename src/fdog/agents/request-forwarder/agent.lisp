(in-package :request-forwarder-agent)
(defcategory request-forwarder-agent)

;; Transform functions
(defgeneric agent-request-transform (agent transform request)
  (:documentation "A generic method for handling `:keyword' transforms
for an agent. Defining methods specializing on `transform' will allow
them to be named in the `transforms' for transformations
that require access to the data accessible through the current agent.")
  (:method (agent transform request)
    "The default transformation action for an unknown transform is `identity'"
    (log-for (warn request-forwarder-agent) "The ~S transform is not defined for ~A" transform agent)
    request))


(defun add-identifier (request)
  "Update the request sender in a manner that
will still match the ZMQ_SUBSCRIBE mask of the sending
mongrel, but such that we can still parse it out
also add it as a request header in case anyone upstream
wants to know it."
  (let ((id (prin1-to-string (uuid:make-v4-uuid))))
    (unless (assoc "x-fdog-request-id" (m2cl:request-headers request) :test #'string-equal)
      (setf (m2cl:request-sender request)
          (concatenate 'string (m2cl:request-sender request) (format nil "--id-~A" id)))
      (push (cons "X-Fdog-Request-ID" id) (m2cl:request-headers request)))
    request))

(defmethod agent-request-transform (agent (transform (eql :strip-prefix)) request)
  "Remove the prefix of request as configured in the agent."
  (let ((prefix-re (format nil "^~A" (path agent))))
    (setf (m2cl:request-path request)
          (ppcre:regex-replace prefix-re (m2cl:request-path request) "/"))

    (when-bind path-hdr (assoc :PATH (m2cl:request-headers request))
      (setf (cdr path-hdr)
            (ppcre:regex-replace prefix-re (cdr path-hdr) "/")))

    (when-bind uri-hdr (assoc :URI (m2cl:request-headers request))
      (setf (cdr uri-hdr)
            (ppcre:regex-replace prefix-re (cdr uri-hdr) "/")))

    request))

;; Agent
(defclass request-forwarder-agent (request-processing-agent standard-leaf-agent)
  ((forwarder :initform "x-NO-forwarder"
              :initarg :forwarder
              :accessor forwarder)
   (route :initform "x-NO-route"
          :initarg :route
          :accessor route)
   (path :initform ""
         :accessor path)

   (transforms :initform (list 'add-identifier
                               :strip-prefix)
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
    (when-bind routes (cdr (assoc :routes (rest forwarder)))
      (log-for (request-forwarder-agent trace) "Looking for path in forwarder-routes: ~S" routes)
      (when-bind route (assoc (route agent) routes :key #'cdr :test #'string=)
        (when-bind my-path (cdr (assoc :route route))
          ;; Only perform the requesticle toggle
          ;; when we see a path for our forwarder
          ;; that doesn't match ours
          (unless (string= (path agent) my-path)
            (log-for (request-forwarder-agent trace) "My path is now: ~S" my-path)
            (setf (path agent) my-path)
            (send-message organ :command
                          `(:command :requesticle
                            :requesticle :enable))))))))
