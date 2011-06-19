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
