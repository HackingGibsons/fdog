(in-package :agent)

;; Makers
(defmethod make-heart-for ((agent standard-agent))
  (let ((heart (make-instance 'agent-heart :agent agent)))
    heart))

(defmethod make-appendix-for ((agent standard-agent))
  (let ((appendix (make-instance 'agent-appendix :agent agent)))
    appendix))

(defun make-agent ()
  "Agent maker wrapper"
  (let* ((agent (make-instance 'standard-agent))
         (heart (make-heart-for agent))
         (appendix (make-appendix-for agent)))
    (agent-connect agent heart)
    (agent-connect agent appendix)
    agent))

