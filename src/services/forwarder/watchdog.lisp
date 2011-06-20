(in-package :fdog-forwarder)

(defclass mongrel2-watchdog ()
  ((server :initarg :server
           :initform nil)
   (route :initarg :route
          :initform nil)
   (bridge :initform nil
           :initarg :bridge
           :accessor mongrel2-watchdog-bridge))
  (:documentation "A mongrel2 server watchdog to ensure the server is always up and responding."))

(defmethod mongrel2-watchdog-running-p ((watchdog mongrel2-watchdog))
  (and (mongrel2-watchdog-bridge watchdog)
       (request-handler-running-p (mongrel2-watchdog-bridge watchdog))))

(defmethod mongrel2-watchdog-start ((watchdog mongrel2-watchdog))
  (unless (mongrel2-watchdog-running-p watchdog)
    (request-handler-start (mongrel2-watchdog-bridge watchdog))))

(defmethod mongrel2-watchdog-stop ((watchdog mongrel2-watchdog))
  (when (mongrel2-watchdog-running-p watchdog)
    (request-handler-stop (mongrel2-watchdog-bridge watchdog))))

(defmethod make-mongrel2-watchdog ((server mongrel2-server))
  (let* ((route (ensure-server-has-watchdog server))
         (watchdog (make-instance 'mongrel2-watchdog
                                  :server server :route route
                                  :bridge (configure-bridges-for (mongrel2-route-target route)))))
    watchdog))

(defmethod initialize-instance :after ((watchdog mongrel2-watchdog) &rest initargs)
  "Post-initialization setup.
TODO: This was mostly a sketch-up to test SSL and the scaffolding machinery.
      the test was successfull, so I'll get back to it."
  (log-for (trace) "After init for: ~A Bridge: ~A" watchdog (mongrel2-watchdog-bridge watchdog))
  (request-handler-add-string-responder (mongrel2-watchdog-bridge watchdog)
                                        (lambda (request)
                                          (log-for (trace) "Serving request")
                                          (format nil "~A" (current-thread)))))
