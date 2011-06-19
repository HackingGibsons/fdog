(in-package :fdog-forwarder)

(defun forwarder-servers ()
  "A list of all servers that are configured for forwarding"
  (multiple-value-list (ensure-forwarder-servers-exist)))

(defmethod configure-forwarder ((forwarder fdog-forwarder) (server mongrel2-server))
  "Configure `forwarder' on `server'"
  (log-for (trace) "Configuring ~A on ~A" forwarder (fdog-models:mongrel2-server-name server))
  (log-for (trace) "Making handlers for unique paths")
  (flet ((send-ident-for (path) (format nil "forwarder-~A-~A" (fdog-forwarder-name forwarder)
                                                              path)))
    (dolist (path (forwarder-uniqe-paths forwarder))
      (log-for (trace) "Building forwarder handler for ~A" path)
      (make-mongrel2-handler (send-ident-for path)
                             (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))
                             (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port)))))
  forwarder)


(defmethod configure-forwarder ((forwarder fdog-forwarder) (server (eql :all)))
  "Configure all forwarder servers to include `forwarder'"
  (log-for (trace) "Configuring ~A on all forwarder servers" forwarder)
  (mapcar #'(lambda (s) (configure-forwarder forwarder s))
          (forwarder-servers)))

