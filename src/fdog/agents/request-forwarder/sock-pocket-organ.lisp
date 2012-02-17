(in-package :request-forwarder-agent)

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
  (declare (ignorable options))
  "Build the default endpoint and assign it an address."
  (setf (gethash :default (client-socks organ))
        (make-instance 'forwarder-endpoint :agent agent :organ organ
                       :name :default)))

(defmethod agent-disconnect :after ((agent standard-agent) (organ agent-sock-pocket) &rest options)
  (declare (ignorable options))
  "Disconnect all the connected client endpoints."
  (flet ((dispose-endpoint (key endpoint)
           (dispose endpoint)
           (remhash key (client-socks organ))))
    (maphash #'dispose-endpoint (client-socks organ))))

(defmethod client-endpoint ((organ agent-sock-pocket) name)
  "Helper to pull out an endpoint named `name' from the
sock-pocket named by `organ'"
  (gethash name (client-socks organ)
           (gethash :default (client-socks organ))))

;; Event loop registrations
(defmethod writer-callbacks :around ((organ agent-sock-pocket))
  (multiple-value-bind (socks callbacks) (call-next-method)
    (maphash #'(lambda (name endpoint)
                 (declare (ignorable name))
                 (unless (push-ready-p endpoint)
                   (appendf socks (list (sock-of (push-sock endpoint))))
                   (appendf callbacks (list #'(lambda (sock)
                                                (declare (ignore sock))
                                                (push-ready endpoint))))))
             (client-socks organ))
    (values socks callbacks)))
