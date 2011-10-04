(in-package :agent)

;; Makers
(defmethod make-heart-for ((agent standard-agent))
  (let ((heart (make-instance 'agent-heart :agent agent)))
    heart))

(defmethod make-appendix-for ((agent standard-agent))
  (let ((appendix (make-instance 'agent-appendix :agent agent)))
    appendix))

(defmethod make-head-for ((agent standard-agent))
  (let ((head (make-instance 'agent-head :agent agent)))
    (make-announce-self head)
    head))

(defmethod make-mouth-for ((agent standard-agent))
  (let ((mouth (make-instance 'agent-mouth :agent agent)))
    (make-speak-when-told mouth)
    mouth))

(defmethod make-ear-for ((agent standard-agent))
  (let ((ear (make-instance 'agent-ear :agent agent)))
    ear))

(defun make-agent ()
  "Agent maker wrapper"
  (let* ((agent (make-instance 'standard-agent))
         (head (make-head-for agent))
         (heart (make-heart-for agent))
         (mouth (make-mouth-for agent))
         (ear (make-ear-for agent))
         (appendix (make-appendix-for agent)))

    (agent-connect agent mouth)
    (agent-connect agent ear)
    (agent-connect agent head)
    (agent-connect agent heart)
    (agent-connect agent appendix)

    agent))
