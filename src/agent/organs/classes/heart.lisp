(in-package :agent)

(defclass agent-heart (standard-organ)
  ((beat-every :initarg :beat-every
               :accessor heart-beat-every
               :initform 0.3)

   (last-beat :initform 0
              :accessor heart-last-beat)
   (next-beat :initform (get-internal-real-time)
              :accessor heart-next-beat)))
