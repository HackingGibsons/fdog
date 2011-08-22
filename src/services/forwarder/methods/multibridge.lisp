(in-package :fdog-forwarder)

;; Multibridge Construction and init
(defmethod make-multibridge ((engine forwarder-engine) (handler mongrel2-handler))
  (log-for (trace) "Building multibridge for: ~A" handler)
  (make-instance 'multibridge :engine engine :handler handler
                 :path (mongrel2-route-path (mongrel2-target-route handler))))

;; Multibridge Operation
(defmethod multibridge-running-bridges ((instance multibridge))
  (remove-if-not #'request-handler-running-p (multibridge-bridges instance)))

(defmethod multibridge-idle-bridges ((instance multibridge))
  (remove-if #'request-handler-running-p (multibridge-bridges instance)))

(defmethod multibridge-start ((instance multibridge))
  (unless (multibridge-bridges instance)
    (log-for (trace) "No bridges present, but asked to start. Adding bridge.")
    (dotimes (x 1)
      (multibridge-add-bridge instance)))
  (mapc #'request-handler-start (multibridge-idle-bridges instance))
  instance)

(defmethod multibridge-stop ((instance multibridge))
  (mapc #'request-handler-stop (multibridge-running-bridges instance))
  instance)

(defmethod multibridge-configure-new-bridge ((instance multibridge) (bridge fdog-handler:request-handler))
  (log-for (trace) "Configuring bridge: ~A" bridge)

  (let* ((endpoint (forwarder-engine-endpoint (multibridge-engine instance)))
         (processors (multibridge-request-proccessors instance)))

    (flet ((handler-closure (handler request raw)
             (let ((last (list handler request raw)))
               (dolist (proc processors last)
                 (log-for (dribble) "Processing with: ~A" proc)
                 (setf last (apply proc last))))))

      (setf (request-handler-processors bridge) `(,#'handler-closure))
      (log-for (trace) "Set request-handler callchain entirely to the forwarder closure.")))
  bridge)

(defmethod multibridge-add-bridge ((instance multibridge))
  (log-for (trace) "Adding bridge to ~A" instance)
  (let ((bridge (configure-bridges-for (multibridge-handler instance))))
    (multibridge-configure-new-bridge instance bridge)
    (push bridge (multibridge-bridges instance))))

(defmethod multibridge-running-p ((instance multibridge))
  (with-slots (bridges) instance
    (and bridges
         (remove-if-not #'request-handler-running-p bridges))))

