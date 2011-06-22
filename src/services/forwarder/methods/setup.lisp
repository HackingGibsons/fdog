(in-package :fdog-forwarder)

(defun forwarder-servers ()
  "A list of all servers that are configured for forwarding"
  (ensure-forwarder-servers-exist))

(defun send-ident-for (forwarder path &optional ssl)
  (ppcre:regex-replace-all "/"
                           (format nil "forwarder-~A-~A~:[~;-ssl~]"
                                   (fdog-forwarder-name forwarder) path ssl)
                           "SLASH"))

(defmethod configure-forwarder ((forwarder fdog-forwarder) (server mongrel2-server))
  "Configure `forwarder' on `server'"
  (log-for (trace) "Configuring ~A on ~A" forwarder (fdog-models:mongrel2-server-name server))
  (log-for (trace) "Making handlers for unique paths")
  (dolist (path (forwarder-uniqe-paths forwarder))
    (log-for (trace) "Building forwarder handler for ~A" path)
    (make-mongrel2-handler (send-ident-for forwarder path (mongrel2-server-ssl-p server))
                           (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))
                           (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))
                           :recv-ident (send-ident-for forwarder path (mongrel2-server-ssl-p server))
                           :update nil))

  (log-for (trace) "Configuring routes for each host to a handler")
  (dolist (hostpath (fdog-forwarder-hostpaths forwarder))
    (log-for (trace) "Routing ~A~A" (fdog-hostpath-host hostpath)
             (fdog-hostpath-path hostpath))
    (let* ((host (make-mongrel2-host server (fdog-hostpath-host hostpath)))
           (target (find-mongrel2-handler :send-ident (send-ident-for forwarder (fdog-hostpath-path hostpath) (mongrel2-server-ssl-p server))))
           (route (make-host-route host (fdog-hostpath-path hostpath) target)))
      (log-for (trace) "Will use host: ~A" host)
      (log-for (trace) "Will use target: ~A" target)
      (log-for (trace) "Will use route: ~A" route)))

  forwarder)

(defmethod configure-forwarder ((forwarder fdog-forwarder) (server (eql :all)))
  "Configure all forwarder servers to include `forwarder'"
  (log-for (trace) "Configuring ~A on all forwarder servers" forwarder)
  (mapcar #'(lambda (s) (configure-forwarder forwarder s))
          (forwarder-servers)))

(defmethod configure-forwarder ((forwarder (eql :all)) server)
  "Helper to configure all forwarders"
  (dolist (forwarder (find-forwarder))
    (configure-forwarder forwarder server)))
