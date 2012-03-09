(in-package :forwarder-agent)

;; Agent
(defclass forwarder-agent (standard-manager-agent rooted-agent-mixin)
  ((forwarders
    :accessor forwarders
    :initform nil
    :documentation "A list of forwarders this agent provides. Elements take the form (name . (list-of-metadata))")
   (forwarder-agents
    :accessor forwarder-agents
    :initform nil
    :documentation "An alist mapping forwarder names and routs to request forwarder agents."))
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

(defmethod find-forwarder-by-name ((agent forwarder-agent) name)
  (assoc name (forwarders agent) :test #'string=))

(defmethod routes-for-forwarder ((agent forwarder-agent) name)
  (awhen (cdr (find-forwarder-by-name agent name))
    (cdr (assoc :routes it))))

(defmethod save-forwarders ((agent forwarder-agent))
  (let* ((forwarder-file (forwarder-file-path agent))
         (tmp-file (forwarder-file-path-tmp agent))
         (forwarder-list (forwarders agent)))
    (with-open-file (out tmp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (json:encode-json-alist forwarder-list out))
    (rename-file tmp-file forwarder-file)))

;; Hooks
(defmethod agent-provides :around ((agent forwarder-agent))
  (append (call-next-method) `(:forwarders ,(forwarders agent))))

(defmethod agent-special-event :after ((agent forwarder-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (with-open-file (in (forwarder-file-path agent) :if-does-not-exist nil)
    (when in
      (dolist (forwarder (load-forwarder-json in))
        (agent-needs agent (find-organ agent :head) :forwarder (cadr forwarder))))))

;; Helpers
(defun load-forwarder-json (stream)
  "Deserializes forwarder.json stream into a format expected by the `forwarder-agent''s
`forwarders' slot.

Only decodes JSON if the file exists and is not empty.

Side effect: All forwarder names will be lowercased"
  (unless (zerop (file-length stream))
    (mapcar #'(lambda (x) (list (string-downcase (symbol-name (car x))) (cdr x))) (json:decode-json stream))))
