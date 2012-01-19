(in-package :forwarder-agent)

;; Agent
(defclass forwarder-agent (standard-leaf-agent rooted-agent-mixin)
  ((forwarders
    :accessor forwarders
    :documentation "A list of forwarders this agent provides. Elements take the form (name . (list-of-metadata))"))
  (:documentation "Fdog forwarder Agent."))

;; Helpers
(defmethod add-forwarder ((agent forwarder-agent) forwarder &optional metadata)
  (with-slots (forwarders) agent
    (push forwarders (cons forwarder metadata))
    ;; TODO persistence
    ))

(defmethod remove-forwarders ((agent forwarder-agent) forwarders)
  (with-slots (forwarders) agent
    (setf forwarders (remove-if #'(lambda (x) (find (car x) forwarder :test #'string=)) forwarders))
    ;; TODO persistence
    ))

(defmethod cull-forwarders ((agent forwarder-agent) forwarders-to-keep)
  (with-slots (forwarders) agent
    (setf forwarders (remove-if-not #'(lambda (x) (find (car x) forwarder :test #'string=)) forwarders))
    ;; TODO persistence
    ))

;; Hooks
(defmethod agent-provides :around ((agent forwarder-agent))
  (append (call-next-method) `(:forwarders ,(forwarders agent))))

(defmethod agent-special-event :after ((agent forwarder-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  ;; TODO persistence load
  )
