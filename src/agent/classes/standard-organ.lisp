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

(defclass standard-beating-organ (standard-organ)
  ((keep-beats :initform 5
               :accessor keep-beats)
   (last-beat :accessor last-beat)
   (tag :initform :anonymous-organ
        :initarg :tag
        :accessor organ-tag
        :documentation "A beating organ heads it's heartbeat reply with a keyword from this slot.")))

(defmethod initialize-instance :after ((organ standard-beating-organ) &key)
  "Pad the `last-beat' list with nils"
  (setf (last-beat organ) (make-list (keep-beats organ) :initial-element nil)))

(defmethod organ-tag ((organ t))
  "An untagged `organ' should look like it's type. Makes mapping across them easier."
  (intern (symbol-name (type-of organ)) :keyword))

(defcategory organ)