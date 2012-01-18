(in-package :forwarder-agent)

;; Agent
(defclass forwarder-agent (rooted-agent-mixin)
  ()
  (:documentation "Fdog forwarder Agent."))

;; Helpers

;; Hooks
(defmethod agent-provides :around ((agent forwarder-agent))
  ;; TODO announce forwarders - is it okay to store them in a slot?
  (call-next-method))

(defmethod agent-special-event :after ((agent forwarder-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  ;; TODO persistence load
  )
