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

(defmethod update-instance-for-different-class :after ((previous agent-runner) (runner host-runner) &key)
  "Handles the upgrade to the `host-runner' class of instances of another runner.
Called as a result of `change-class' in `make-runner' specialized on `:host'"
  (log-for (agent-runner) "Creating `host-runner' instance: ~A" (agent-instance runner))

  ;; Find and bind an agent-host, store it globally if it isn't yet
  (unless (agent-handle runner)
    (setf (agent-handle runner)
          (or *agent-host* (make-instance 'agent-host))))
  (unless *agent-host*
    (log-for (warn agent-runner) "`*agent-host*' is nil. Binding to ~A" (agent-handle runner))
    (setf *agent-host* (agent-handle runner))))

;; Methods
(defmethod stop ((runner host-runner))
  (remove-agent (agent-handle runner) (agent-instance runner)))

(defmethod start ((runner host-runner) &key (category '(log5:warn+)))
  ;; TODO: Logging has been disabled until it's fast not
  ;;       costing minutes per build and failing tests :(
  (start-logging :category category)

  (add-agent (agent-handle runner) (agent-instance runner))
  (if (running-p (agent-handle runner))
      (agent-handle runner)
      (run (agent-handle runner))))


(defmethod running-p ((runner host-runner))
  "Ask the container if it's running."
  (and (running-p (agent-handle runner))
       (has-agent-p (agent-handle runner) (agent-instance runner))))
