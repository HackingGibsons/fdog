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

(defmethod initialize-instance :after ((endpoint forwarder-endpoint) &key)
  "Bind the addresses of the given endpoint."
  (bind endpoint))

(defmethod push-ready-p ((endpoint forwarder-endpoint))
  "Shortcut to check of `push-state' of the `endpoint' is `:ready'"
  (eql (push-state endpoint) :ready))

(defmethod push-ready ((endpoint forwarder-endpoint))
  "Set an endpoint as ready."
  (log-for (trace forwarder-endpoint) "Readying endpoint: ~A" endpoint)
  (setf (push-state endpoint) :ready))

(defmethod push-unready ((endpoint forwarder-endpoint))
  "Set the endpoint as not ready."
  (setf (push-state endpoint) :not-ready))

(defmethod make-push-callback ((endpoint forwarder-endpoint))
  "Return a function that signals that this `endpoint's :push
socket is ready for IO."
  (lambda (sock)
    (declare (ignorable sock))
    (push-ready endpoint)))

;; Helper
(defmacro sock-of (x) `(aref ,x 0))
(defmacro addr-of (x) `(aref ,x 1))

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
