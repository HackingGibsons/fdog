(in-package :agent)

;; Makers
(defun make-agent ()
  "Agent maker wrapper"
  (let* ((agent (make-instance 'standard-agent)))
    agent))
