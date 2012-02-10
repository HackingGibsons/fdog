(in-package :request-forwarder-agent)

;; Client endpoint
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

;; Setup and tear down methods
(defmethod bind ((endpoint forwarder-endpoint))
  :TODO-bind-addrs)

(defmethod dispose ((endpoint forwarder-endpoint))
  "Tear down the sockets and nil out all the socket related slots."
  (macrolet ((sock-of (x) `(aref ,x 0))
             (addr-of (x) `(aref ,x 1)))
    ;; Close
    (when (sock-of (push-sock endpoint))
      (zmq:close (push-sock endpoint)))
    (when (sock-of (sub-sock endpoint))
      (zmq:close (sub-sock endpoint)))

    ;; Nil out the slots
    (setf (push-sock endpoint) #(nil nil)
          (sub-sock endpoint) #(nil nil)
          (name endpoint) nil)))
