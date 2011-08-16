(in-package :fdog-forwarder)

(defun ensure-forwarder-environment ()
  "Ensures the forwarder tables and the forwarder servers are created
and watchdogs are installed. Returns the forwarding servers."
  (ensure-forwarder-tables-exist)
  (mapc #'ensure-server-has-watchdog (ensure-forwarder-servers-exist)))

(defun ensure-forwarder-table-exists (table)
  (if (clsql:table-exists-p (clsql:view-table (find-class table)))
      (log-for (trace) "Already have table for: ~A" table)

      (prog1 (clsql:create-view-from-class table)
        (log-for (trace) "Made table for: ~A" table))))

(defun ensure-forwarder-tables-exist ()
  (mapcar #'ensure-forwarder-table-exists
          '(fdog-forwarder fdog-forwarder-hostpath fdog-forwarder-queue)))

(defun ensure-forwarder-servers-exist ()
  "Ensure that the servers required for forwarder operation exist
and are configured to the current environment configuration as dicated by
`*forwarder-server-name*', `*forwarder-server-port*' and `*forwarder-server-ssl-port*'"
  (list (ensure-server-exists :port *forwarder-server-port* :ssl nil)))

(defun ensure-server-exists (&key (name *forwarder-server-name*) (port *forwarder-server-port*) (bind "0.0.0.0") (ssl nil))
  "Ensure that the `named'-ed forwarder server exists with the options given by `port' `bind' and `ssl'"
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
        (handler (make-mongrel2-handler (format nil "watchdog-~A" (mongrel2-server-name server))
                                        (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))
                                        (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))
                                        :update nil)))
    (make-host-route host *watchdog-route* handler)))

(defmethod ensure-servers-running ((server mongrel2-server))
  "Make sure the server `server' is running."
  (log-for (trace) "Ensuring the running state of ~A" server)
  (if (mongrel2-server-running-p server)
      (progn
        (log-for (trace) "Server is running. Asking for reload.")
        (mongrel2-server-signal server :reload))
      (progn
        (log-for (trace) "Server is not running. Starting.")
        (mongrel2-server-signal/block server :start :timeout 3)))

  (unless (mongrel2-server-running-p server)
    (error "Mongrel2 server ~A [~A] failed to start!" (model-pk server) (mongrel2-server-name server)))

  server)

(defmethod ensure-servers-running ((servers list))
  "Make sure that each of the servers in `servers' is running.
If `reload' is true, a reload signal will be sent to the server."
  (log-for (trace) "Ensuring the running state of ~A servers" (length servers))
  (mapcar #'ensure-servers-running servers))
