(in-package :fdog-forwarder)

(defun ensure-forwarder-tables-exist ()
  (if (and (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder)))
               (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder-hostpath))))
      (log-for (trace) "Forwarder tables already exist.")
      (progn
        (log-for (trace) "Forwarder tables do not exist.. creating.")
        (mapcar #'clsql:create-view-from-class '(fdog-forwarder fdog-forwarder-hostpath)))))

(defun ensure-handler-for-forwarder (forwarder &key host server)
  (with-slots (path listen-on forward-to) forwarder
    (let* ((server (or server (ensure-server-exists forwarder *forwarder-server-name* *forwarder-server-port*)))
           (host (or host (mongrel2-server-default-host server)))
           (routes (mongrel2-host-routes host))
           (route (or (find path routes
                            :test #'string= :key #'mongrel2-route-path)

                      (let ((r (make-instance 'mongrel2-route :path path :host-id (model-pk host))))
                        (clsql:update-records-from-instance r)
                        r))))

      (unless (mongrel2-route-target route)
        (setf (mongrel2-route-target route)
              (make-handler :send-spec (make-handler-send-spec)
                            :send-ident (make-handler-send-ident forwarder)
                            :recv-spec (make-handler-recv-spec)
                            :recv-ident (make-handler-recv-ident forwarder)))
        (clsql:update-records-from-instance route))

      (mongrel2-route-target route))))

(defun ensure-server-exists (forwarder name port &key (bind "0.0.0.0"))
  (log-for (trace) "Making sure we have a server named ~A binding to ~A" name port)
  (let* ((server (or (servers :name name :refresh t :one t)
                     (make-server name :port port :bind bind
                                  :default-host (slot-value forwarder 'host))))
         (host (ensure-server-default-host-exists server)))
    (log-for (dribble) "Server with name so far: ~A" server)
    server))

(defmethod ensure-server-default-host-exists ((server mongrel2-server))
  (log-for (trace) "Making sure we have a default host for ~A" server)
  (let* ((host (mongrel2-server-default-host server))
         (correct (and host
                       (= (model-pk server)
                          (model-pk (mongrel2-host-server host))))))
    (unless correct
      (setf host (make-instance 'mongrel2-host
                                :name (mongrel2-server-default-host-name server)
                                :server-id (model-pk server)))
      (clsql:update-records-from-instance host))
    host))
