(in-package :fdog-control)

(defparameter *control-interface* nil
  "The interface for interacting with FDOG externally")

(defun mount-control-application (bridge)
  (log-for (trace) "Mounting control application bridge: ~A" bridge)
  (flet ((response (r)
           (format nil "~A:~A" (get-universal-time) (current-thread)))

         (chunk-info/start (request)
           (declare (ignorable request))
           '((:X-hello-world . "I am awesome")
             ("Trailer" . "X-Magic")))

         (chunk-trailer (request)
           `(("X-Magic" . "Totally")))

         (chunk-two (request)
           (declare (ignorable request))
           (values "  Verily, this is the second reply!"
                   (not (= (random 10) 0)))))
    (request-handler-add-chunked/trailer bridge #'chunk-trailer)
    (request-handler-add-chunked/stop bridge)

    (request-handler-add-chunked/chunk bridge #'chunk-two)
    (request-handler-add-chunked/chunk bridge #'response)

    (request-handler-add-chunked/start bridge #'chunk-info/start)))

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

    (interface-start-server interface)
    (interface-start-bridges *control-interface*))

  *control-interface*)

