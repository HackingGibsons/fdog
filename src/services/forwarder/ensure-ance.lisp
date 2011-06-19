(in-package :fdog-forwarder)

(defun ensure-forwarder-tables-exist ()
  (if (and (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder)))
           (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder-hostpath))))
      (log-for (trace) "Forwarder tables already exist.")
      (progn
        (log-for (trace) "Forwarder tables do not exist.. creating.")
        (mapcar #'clsql:create-view-from-class '(fdog-forwarder fdog-forwarder-hostpath)))))

(defun ensure-forwarder-servers-exist ()
  (values (ensure-server-exists :port *forwarder-server-port* :ssl nil)
          (ensure-server-exists :name (concatenate 'string *forwarder-server-name* "-ssl")
                                :port *forwarder-server-ssl-port* :ssl t)))

(defun ensure-server-exists (&key (name *forwarder-server-name*) (port *forwarder-server-port*) (bind "0.0.0.0") (ssl nil))
  "Ensure that the forwarder server exists"
  (let ((server (or (servers :name name :refresh t :one t)
                    (make-server name :port port :bind bind
                                 :default-host "localhost"))))
    (setf (mongrel2-server-port server) port
          (mongrel2-server-addr server) bind
          (mongrel2-server-ssl server)  (if ssl 1 0))
    (clsql:update-records-from-instance server)
    server))

(defmethod ensure-server-has-default-host-named ((server mongrel2-server) name)
  "Ensures that the default host of `server' exists and is named `name'"
  (clsql:update-instance-from-records server)
  (clsql:update-objects-joins `(,server))
  (let ((host (or (mongrel2-server-default-host server)
                  (make-instance 'mongrel2-host :server-id (model-pk server)
                                 :name name))))

    (setf (mongrel2-host-name host) name
          (mongrel2-host-matching host) name)

    (clsql:update-records-from-instance host)
    host))

(defmethod ensure-server-has-watchdog ((server mongrel2-server))
  "Ensure that `server' has a default route wired to the watchdog
handler"
  (let ((host (ensure-server-has-default-host-named server "localhost"))
        (handler (make-mongrel2-handler "forwarder-watchdog" (cdr (assoc :send *watchdog-endpoints*))
                                                             (cdr (assoc :recv *watchdog-endpoints*)))))
    (make-host-route host *watchdog-route* handler)))
