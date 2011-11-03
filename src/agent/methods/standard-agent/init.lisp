(in-package :agent)

(defgeneric initialize-instance-organs (agent)
  (:documentation "Initalizes the organ loadout of a given agent.")
  (:method ((agent standard-agent))
    nil)
  (:method :after ((agent standard-agent))
           "Default loadout for `standard-agent'"
           (flet ((make-organ (organ-class)
                    (let ((organ (make-instance organ-class :agent agent)))
                      (agent-connect agent organ))))
             (mapc #'make-organ `(agent-head
                                  agent-heart
                                  agent-mouth
                                  agent-ear
                                  agent-eye
                                  agent-hand
                                  agent-appendix)))))
