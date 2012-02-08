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

;; Organ
(defcategory agent-sock-pocket)
(defclass agent-sock-pocket (standard-beating-organ)
  ((client-socks :initform (make-hash-table :test 'equalp)
                 :accessor client-socks))
  (:default-initargs . (:tag :sock-pocket))
  (:documentation "Stores a mapping of destinations to forward requests
to. The default destination is to be called `:default'. Objects stored
`endpoint' instances."))

(defmethod agent-boot :after ((agent standard-agent) (organ agent-sock-pocket) &rest options)
  "Build the default endpoint and assign it an address."
  (setf (gethash :default (client-socks organ))
        (make-instance 'forwarder-endpoint :agent agent :organ organ
                       :name :default)))

(defmethod agent-disconnect :after ((agent standard-agent) (organ agent-sock-pocket) &rest options)
  "Disconnect all the connected client endpoints."
  (flet ((dispose-endpoint (key endpoint)
           (dispose endpoint)
           (remhash key (client-socks organ))))
    (maphash #'dispose-endpoint (client-socks organ))))
