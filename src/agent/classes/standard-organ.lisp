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
  ((last-beat :initform '(nil nil)
              :accessor last-beat)
   (tag :initform :anonymous-organ
        :initarg :tag
        :accessor organ-tag
        :documentation "A beating organ heads it's heartbeat reply with a keyword from this slot.")))

(defmethod organ-tag ((organ t))
  "An untagged `organ' should look like it's type. Makes mapping across them easier."
  (intern (symbol-name (type-of organ)) :keyword))
