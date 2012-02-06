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
  (let ((forwarder-file (forwarder-file-path agent))
        (forwarder-list (mapcar #'(lambda (x) `(,(car x) ,(alexandria:plist-alist (cdr x)))) (forwarders agent))))
    (with-open-file (out forwarder-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (json:encode-json-alist forwarder-list out))))

;; Hooks
(defmethod agent-provides :around ((agent forwarder-agent))
  (append (call-next-method) `(:forwarders ,(forwarders agent))))

(defmethod agent-special-event :after ((agent forwarder-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (with-open-file (in (forwarder-file-path agent) :if-does-not-exist nil)
    (when in
      (dolist (forwarder (load-forwarder-json in))
        (agent-needs agent (find-organ agent :head) :forwarder forwarder)))))

;; Helpers
(defun load-forwarder-json (stream)
  "Deserializes forwarder.json stream into a format expected by the `forwarder-agent''s
`forwarders' slot. For each forwarder key in the JSON, converts the metadata from an alist
(as presented by CL-JSON) to a plist, then converts the hostpaths from
(:API.EXAMPLE.COM . \"/path/\") to (\"api.example.com\" . \"/path/\").

Only decodes JSON if the file exists and is not empty."
  (unless (zerop (file-length stream))
    (let ((forwarders nil))
      (dolist (forwarder (json:decode-json stream))
        (let ((forwarder-plist (alexandria:alist-plist (cadr forwarder))))
          (setf (getf forwarder-plist :hostpaths) (mapcar #'(lambda (x) `(,(string-downcase (symbol-name (car x))) . ,(cdr x))) (getf forwarder-plist :hostpaths)))
          (alexandria:appendf forwarders (list forwarder-plist))))
      forwarders)))
