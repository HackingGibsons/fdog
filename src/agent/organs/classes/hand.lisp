(in-package :agent)

(defcategory hand)

(defclass agent-hand (standard-beating-organ)
  ()
  (:documentation "`standard-agent' hand. For making things.")
  (:default-initargs . (:tag :hand)))


(defmethod initialize-instance :after ((hand agent-hand) &rest initargs)
  (make-spawn-things hand))
