(in-package :agent)

(defclass agent-mouth (standard-beating-organ)
  ()

  (:documentation "Responsible for outgoing cross-agent communication.")
  (:default-initargs . (:tag :mouth)))


(defmethod agent-boot ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific socket inits."
  (declare (ignorable options))
  (log-for (warn) "TOOD: Booting mouth: ~A from ~A" mouth agent))

(defmethod agent-disconnect :after ((agent standard-agent) (mouth agent-mouth) &rest options)
  "Mouth specific disconnect."
  (declare (ignorable options))
  (log-for (warn) "TODO: Disconnecting mouth: ~A from ~A" mouth agent))
