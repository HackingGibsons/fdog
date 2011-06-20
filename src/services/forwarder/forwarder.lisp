(in-package :fdog-forwarder)

;; General forwarders API
(defmethod init-forwarders (&key (start t))
  (let ((forwarder-servers (ensure-forwarder-environment))
        (forwarders (find-forwarder)))

    (log-for (trace) "Stopping all ~A existing forwarders" (length *forwarders*))
    (forwarder-engine-stop :all)

    (log-for (trace) "Configuring all ~A found forwarders" (length forwarders))
    (configure-forwarder :all :all)

    (setf *forwarders* (mapcar #'(lambda (f) (make-forwarder-engine f :servers forwarder-servers))
                               (remove-if-not #'forwarder-valid-p forwarders)))
    (log-for (trace) "Built ~A/~A engines" (length *forwarders*) (length forwarders))

    (when start
      (log-for (trace) "Starting all forwarder engines")
      (forwarder-engine-start :all))))
