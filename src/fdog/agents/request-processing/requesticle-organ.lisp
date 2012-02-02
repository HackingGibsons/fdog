(in-package :request-processing-agent)

(defcategory requesticle)
(defclass agent-requesticle (standard-beating-organ)
  ((connected-to :initform (make-hash-table :test 'equalp)
                 :accessor connected-to
                 :documentation "Stores a mapping of strings representing connected-to peers
The table stores connections in the form send-addr->host-agent-uuid as strings.
Just the send address is used as a key but connections are made to both send and recv socks")
   (handler :initform (make-instance 'm2cl:handler :pub nil :pull nil)
            :accessor handler
            :initarg :handler))
  (:documentation "Responsible for firing events when Mongrel2 requests arrive
at the request sock.")
  (:default-initargs . (:tag :requesticle)))

(defmethod request-sock ((organ agent-requesticle))
  "Pass through read access to the request socket"
  (m2cl:handler-pull-socket (handler organ)))
(defmethod set-request-sock ((organ agent-requesticle) v)
  "Pass through write access to the request socket"
  (setf (m2cl:handler-pull-socket (handler organ)) v))
(defsetf request-sock set-request-sock)

(defmethod response-sock ((organ agent-requesticle))
  "Pass through read access to the response socket"
  (m2cl:handler-pull-socket (handler organ)))
(defmethod set-response-sock ((organ agent-requesticle) v)
  "Pass through write access to the response socket"
  (setf (m2cl:handler-pub-socket (handler organ)) v))
(defsetf response-sock set-response-sock)


(defmethod initialize-instance :after ((organ agent-requesticle) &key)
  "Attach the requesticle behaviors."
  (make-have-hearing organ))

(defmethod agent-boot :after ((agent standard-agent) (requesticle agent-requesticle) &rest options)
  "Init the requesticle sock"
  (declare (ignorable options))
  (log-for (trace requesticle) "Requesticle constructing socket.")

  (setf (request-sock requesticle)
        (zmq:socket (agent-context agent) :pull)

        (response-sock requesticle)
        (zmq:socket (agent-context agent) :pub)))

(defmethod agent-disconnect :after ((agent standard-agent) (requesticle agent-requesticle) &rest options)
  "Disconnect the requesticle sock"
  (declare (ignorable options))
  (log-for (trace requesticle) "Requesticle destroying socket.")

  (when (request-sock requesticle)
    (zmq:close (request-sock requesticle)))

  (when (response-sock requesticle)
    (zmq:close (response-sock requesticle)))

  (setf (request-sock requesticle) nil
        (response-sock requesticle) nil))

(defmethod agent-info :around ((organ agent-requesticle))
  `(,(organ-tag organ) (:uuid ,(organ-uuid organ)
                        :peers ,(hash-table-count (connected-to organ)))))
