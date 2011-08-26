(in-package :agent)

(defclass standard-organ ()
  ((agent :initarg :agent
          :accessor organ-agent)
   (uuid :initarg :uuid
         :reader organ-uuid
         :initform (print-object (uuid:make-v4-uuid) nil))

   (incoming-sock :initform nil
                  :accessor organ-incoming-sock)
   (outgoing-sock :initform nil
                  :accessor organ-outgoing-sock)))
