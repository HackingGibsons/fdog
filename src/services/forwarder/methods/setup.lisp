(in-package :fdog-forwarder)

(defun forwarder-servers ()
  "A list of all servers that are configured for forwarding"
  (multiple-value-list (ensure-forwarder-servers-exist)))

(defmethod configure-forwarder ((forwarder fdog-forwarder) (server mongrel2-server))
  "Configure `forwarder' on `server'"
  (log-for (trace) "Configuring ~A on ~A" forwarder (fdog-models:mongrel2-server-name server))
  (dolist (hostpath (fdog-forwarder-hostpaths forwarder))
    (log-for (warn) "TODO: Should configure ~A => ~A"
             (fdog-hostpath-host hostpath)
             (fdog-hostpath-path hostpath))))

(defmethod configure-forwarder ((forwarder fdog-forwarder) (server (eql :all)))
  "Configure all forwarder servers to include `forwarder'"
  (log-for (trace) "Configuring ~A on all forwarder servers" forwarder)
  (mapcar #'(lambda (s) (configure-forwarder forwarder s))
          (forwarder-servers)))

