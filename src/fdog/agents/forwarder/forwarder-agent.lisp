(in-package :forwarder-agent)

;; Agent
(defclass forwarder-agent (standard-leaf-agent rooted-agent-mixin)
  ((forwarders
    :accessor forwarders
    :initform nil
    :documentation "A list of forwarders this agent provides. Elements take the form (name . (list-of-metadata))"))
  (:documentation "Fdog forwarder Agent."))

;; Helpers
(defmethod add-forwarder ((agent forwarder-agent) forwarder &optional metadata)
  (with-slots (forwarders) agent
    (appendf forwarders (list (cons forwarder metadata)))
    (save-forwarders agent)))

(defmethod remove-forwarders ((agent forwarder-agent) names)
  (with-slots (forwarders) agent
    (setf forwarders (remove-if #'(lambda (x) (find (car x) names :test #'string=)) forwarders))
    (save-forwarders agent)))

(defmethod cull-forwarders ((agent forwarder-agent) names-to-keep)
  (with-slots (forwarders) agent
    (setf forwarders (remove-if-not #'(lambda (x) (find (car x) names-to-keep :test #'string=)) forwarders))
    (save-forwarders agent)))

(defmethod save-forwarders ((agent forwarder-agent))
  (let ((forwarder-file (forwarder-file-path agent)))
    (with-open-file (out forwarder-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (json:encode-json-alist (forwarders agent) out))))

;; Hooks
(defmethod agent-provides :around ((agent forwarder-agent))
  (append (call-next-method) `(:forwarders ,(forwarders agent))))

(defmethod agent-special-event :after ((agent forwarder-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  ;; TODO persistence load
  ;; alist-to-json
  ;; foreach forwarder, agent-needs it
  )
