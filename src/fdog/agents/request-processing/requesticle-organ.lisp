(in-package :request-processing-agent)

(defcategory requesticle)
(defclass agent-requesticle (standard-beating-organ)
  ((connected-to :initform (make-hash-table :test 'equalp)
                 :accessor connected-to)
   (request-sock :initform nil
                 :accessor request-sock)
   (response-sock :initform nil
                 :accessor response-sock))
  (:documentation "Responsible for firing events when Mongrel2 requests arrive
at the request sock.")
  (:default-initargs . (:tag :requesticle)))

(defmethod initialize-instance :after ((organ agent-requesticle) &key)
  "Attach the requesticle behaviors."
  (make-have-hearing organ))

(defmethod agent-boot :after ((agent standard-agent) (requesticle agent-requesticle) &rest options)
  "Init the requesticle sock"
  (declare (ignorable options))
  (log-for (trace requesticle) "Requesticle constructing socket.")

  (setf (request-sock requesticle)
        (zmq:socket (agent-context agent) zmq:pull)

        (response-sock requesticle)
        (zmq:socket (agent-context agent) zmq:pub)))

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
