(in-package :agent-host)

(defcategory agent-runner)

(defclass host-runner (agent-runner)
  ()
  (:documentation "Agent runner that uses a shared `agent-host' at `*agent-host*' to
run agents instead of using threads or processes"))

(defmethod make-runner ((style (eql :host)) &key)
  "Upgrade to a host runner."
  (let ((runner (call-next-method)))
    (change-class runner 'host-runner)))

(defmethod initialize-instance :after ((runner host-runner) &rest initargs &key)
  (declare (ignorable initargs))
  (log-for (agent-runner) "Creating `host-runner' instance: ~A/~A" (agent-instance runner) initargs)

  ;; Find and bind an agent-host, store it globally if it isn't yet
  (unless (agent-handle runner)
    (setf (agent-handle runner)
          (or *agent-host* (make-instance 'agent-host))))
  (unless *agent-host*
    (log-for (warn agent-runner) "`*agent-host*' is nil. Binding to ~A" (agent-handle runner))
    (setf *agent-host* (agent-handle runner)))

  (add-agent (agent-handle runner) (agent-instance runner)))
