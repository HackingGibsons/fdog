(in-package :mongrel2-agent)

(defmethod remove-server (server)
  "Remove the given `server' from the database and anything attached to it."
  (let* ((hosts (fdog-models:mongrel2-server-hosts server)))
    (mapc #'remove-host hosts)
    (clsql:delete-instance-records server)
    server))


(defmethod remove-host (host)
  "Remove the given `host' from the database, and anything attached to it."
  (let* ((routes (fdog-models:mongrel2-host-routes host))
         (targets (loop for route in routes
                     appending (fdog-models:mongrel2-route-target route))))
    (log-for (agent-needs trace) "Removing hosts: ~A" host)
    (log-for (agent-needs trace) "Found routes: ~A" routes)
    (log-for (agent-needs trace) "Found targets: ~A" targets)

    (mapc #'clsql:delete-instance-records
          (append (list host) routes targets))))

(defmethod unlink-server ((organ agent::standard-organ) (server fdog-models:mongrel2-server) config)
  (flet ((make-mongrel2-arguments (server config)
           (let ((uuid (fdog-models:mongrel2-server-uuid server)))
             `(:path "/usr/bin/env" :args ("mongrel2" ,(namestring config) ,uuid)))))

    (let ((arguments (make-mongrel2-arguments server config))
          (pid (fdog-models:mongrel2-server-pid server)))

      (log-for (mongrel2-agent trace) "Found mongrel2 server ~A to unlink with make arguments of ~A." server arguments)

      (send-message organ :command `(:command :unlink
                                              :unlink :process
                                              :process (:pid ,pid ,@arguments)))

      (log-for (mongrel2-agent trace) "Stopping server: ~A" server)
      (fdog-models:mongrel2-server-signal server :stop)
      server)))

(defmethod link-server ((organ agent::standard-organ) (server fdog-models:mongrel2-server) config)
  (flet ((make-mongrel2-arguments (server config)
           (let ((uuid (fdog-models:mongrel2-server-uuid server)))
             `(:path "/usr/bin/env" :args ("mongrel2" ,(namestring config) ,uuid)))))

    (let ((arguments (make-mongrel2-arguments server config))
          (pid (fdog-models:mongrel2-server-pid server)))

      (log-for (mongrel2-agent trace) "Found mongrel2 server ~A to link with make arguments of ~A." server arguments)

      (when (fdog-models:mongrel2-server-running-p server)
        (log-for (mongrel2-agent trace) "Reloading server: ~A" server)
        (fdog-models:mongrel2-server-signal server :reload)
        (log-for (mongrel2-agent trace) "Reloaded server: ~A" server))

      (send-message organ :command `(:command :link
                                              :link :process
                                              :process (:pid ,pid ,@arguments)))
      server)))

(defmethod link-all-servers ((organ agent::standard-organ) config)
  "Call (link-server) on every server"
  (mapcar #'(lambda (s) (link-server organ s config))
          (fdog-models:servers :refresh t)))
