(in-package :agent)

(defclass standard-organ ()
  ((agent :initarg :agent
          :accessor organ-agent)
   (uuid :initarg :uuid
         :reader organ-uuid
         :initform (print-object (uuid:make-v4-uuid) nil))))
