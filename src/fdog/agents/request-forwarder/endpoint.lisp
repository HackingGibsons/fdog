(in-package :request-forwarder-agent)

;; Client endpoint
(defcategory forwarder-endpoint)
(defclass forwarder-endpoint ()
  ((organ :initform nil
          :initarg :organ
          :accessor organ)

   (agent :initform nil
          :initarg :agent
          :accessor agent)

   (name :initform :default
         :initarg :name
         :accessor name)

   (push-sock-state :initform :unknown
                    :accessor push-state
                    :accessor push-sock-state)
   (push-sock :initform #(nil nil) :initarg :push
              :accessor push-sock
              :documentation "Stored in the format #(sock addr)")
   (sub-sock :initform #(nil nil) :initarg :sub
             :accessor sub-sock
             :documentation "Stored in the format #(sock addr)"))
  (:documentation "A collection of sockets and addresses
that describe a client facing endpoint for `agent'/`organ'
that has a client endpoint named `name'."))

;; Helpers to navigate the socket vectors
(defmacro sock-of (x) `(aref ,x 0))
(defmacro addr-of (x) `(aref ,x 1))

(defmethod print-object ((endpoint forwarder-endpoint) stream)
  "Pretty printer for the `forwarder-endpoint'"
  (format stream "#<Forwarder-Endpoint[~S] State: ~S Push[~A]:Sub[~A]>"
          (name endpoint) (push-sock-state endpoint)
          (addr-of (push-sock endpoint)) (addr-of (sub-sock endpoint))))

(defmethod initialize-instance :after ((endpoint forwarder-endpoint) &key)
  "Bind the addresses of the given endpoint."
  (bind endpoint))

(defmethod push-ready-p ((endpoint forwarder-endpoint))
  "Shortcut to check of `push-state' of the `endpoint' is `:ready'"
  (eql (push-state endpoint) :ready))

(defgeneric push-state-signal (agent organ endpoint)
  (:documentation "Called when a push socket on `endpoint' endpoint
is signaled in an I/O state transition or noted as such in an IO operation.
Called by the simple-specialization methods `push-ready' and `push-unready'")
  (:method (agent organ (endpoint forwarder-endpoint))
    (if (and (agent endpoint) (organ endpoint))
        (push-state-signal (agent endpoint) (organ endpoint) endpoint)
        (error "Endpoint ~A lacks agent or organ." endpoint)))

  (:method ((agent standard-agent) (organ standard-organ) (endpoint forwarder-endpoint))
    (signal "~A/~A does not care about `push-state-signal' on ~A" agent organ endpoint)))

(defgeneric push-ready (endpoint)
  (:documentation "Set an endpoint as ready.")
  (:method ((endpoint forwarder-endpoint))
    "Main driver method"
    (log-for (trace forwarder-endpoint) "Readying endpoint: ~A" endpoint)
    (setf (push-state endpoint) :ready))

  (:method :after ((endpoint forwarder-endpoint))
    (log-for (trace forwarder-endpoint) "Signaling ready endpoint: ~A" endpoint)
    (push-state-signal (agent endpoint) (organ endpoint) endpoint)))

(defgeneric push-unready (endpoint)
  (:documentation "Set the endpoint as not ready.")
  (:method ((endpoint forwarder-endpoint))
    "Main driver"
    (setf (push-state endpoint) :not-ready))

  (:method :after ((endpoint forwarder-endpoint))
    (log-for (trace forwarder-endpoint) "Signaling unready endpoint: ~A" endpoint)
    (push-state-signal (agent endpoint) (organ endpoint) endpoint)))

(defmethod make-push-callback ((endpoint forwarder-endpoint))
  "Return a function that signals that this `endpoint's :push
socket is ready for IO."
  (lambda (sock)
    (declare (ignorable sock))
    (push-ready endpoint)))

;; Setup and tear down methods
(defmethod bind ((endpoint forwarder-endpoint))
  (let* ((template (format nil
                        "forwarder-~A-~A~-A-~~S"
                        (forwarder (agent endpoint))
                        (route (agent endpoint))
                        (name endpoint)))
         (push-addr (local-address-from-string (format nil template :push) 50000))
         (sub-addr (local-address-from-string (format nil template :sub) 50000)))

    (log-for (forwarder-endpoint trace) "Binding endpoint: ~A" (name endpoint))

    (unless (sock-of (push-sock endpoint))
      (setf (addr-of (push-sock endpoint))
            push-addr

            (sock-of (push-sock endpoint))
            (zmq:socket (agent-context (agent endpoint)) :push))
      (zmq:bind (sock-of (push-sock endpoint)) push-addr))

    (unless (sock-of (sub-sock endpoint))
      (setf (addr-of (sub-sock endpoint))
            sub-addr

            (sock-of (sub-sock endpoint))
            (zmq:socket (agent-context (agent endpoint)) :sub))

      ;; TODO: Get rid of this, probably, and do a proper subscribe
      (zmq:setsockopt (sock-of (sub-sock endpoint)) :subscribe "")

      (zmq:bind (sock-of (sub-sock endpoint)) sub-addr))

    (log-for (forwarder-endpoint trace) "Bound endpoint: ~A" (name endpoint) )))


(defmethod dispose ((endpoint forwarder-endpoint))
  "Tear down the sockets and nil out all the socket related slots."
  ;; Close
  (when (sock-of (push-sock endpoint))
    (zmq:close (push-sock endpoint)))
  (when (sock-of (sub-sock endpoint))
    (zmq:close (sub-sock endpoint)))

  ;; Nil out the slots
  (setf (push-sock endpoint) #(nil nil)
        (sub-sock endpoint) #(nil nil)
        (name endpoint) nil))
