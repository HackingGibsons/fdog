(in-package :request-processing-agent)

(defcategory requesticle)
(defclass agent-requesticle (standard-beating-organ)
  ((connected-to :initform (make-hash-table :test 'equalp)
                 :accessor conneted-to)
   (request-sock :initform nil
                 :accessor request-sock))
  (:documentation "Responsible for firing events when Mongrel2 requests arrive
at the request sock.")
  (:default-initargs . (:tag :requesticle)))

(defmethod initialize-instance :after ((organ agent-requesticle) &key)
  "Attach the requesticle behaviors."
  (make-have-hearing organ))
