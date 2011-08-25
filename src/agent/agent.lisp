(in-package :agent)

;; Makers
(defmethod make-heart-for ((agent standard-agent))
  (let ((heart (make-instance 'agent-heart :agent agent)))
    heart))

(defun make-agent ()
  "Agent maker wrapper"
  (let* ((agent (make-instance 'standard-agent))
         (heart (make-heart-for agent)))
    (agent-connect agent heart)
    agent))

