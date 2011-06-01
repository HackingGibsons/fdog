(in-package :fdog-control)

(defvar *control-interface* nil
  "The interface for interacting with FDOG externally")

(defun mount-control-application (bridge interface)
  (declare (ignore interface))
  (log-for (trace) "Mounting control application on bridge: ~A" bridge)
  (request-handler-add-responder bridge 'root/router :router)
  (log-for (trace) "Mounted dispatcher on bridge."))

(defmethod configure-control-routes (&optional (interface *control-interface*))
  (interface-configure-bridges (interface)
     ("/" :mount-bridge 'mount-control-application)))

(defmethod init-control-interface (&key (server "control"))
  "Creates a new control interface, killing the old, and starts it."
  (when *control-interface*
    (log-for (warn) "Already have a control interface, shutting down..")
    (interface-stop *control-interface*))
  (let ((interface (setf *control-interface* (make-instance 'fdog-interface :server server))))

    (configure-control-routes interface)

    (interface-start interface))

  *control-interface*)

