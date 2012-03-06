(in-package :mongrel2-agent)

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

(defmethod mongrel2-server-handlers ((server fdog-models:mongrel2-server))
  (labels ((remove-duplicate-models (models)
             (remove-duplicates (remove nil models) :test #'equalp :key #'fdog-models:model-pk))

           (server-routes (server)
             (flatten (mapcar #'fdog-models:mongrel2-host-routes
                              (fdog-models:mongrel2-server-hosts server))))

           (server-targets (server)
             (clsql:update-objects-joins `(,server))
             (remove-duplicate-models
              (mapcar #'fdog-models:mongrel2-route-target (server-routes server)))))

    (remove-if-not (rcurry #'typep 'fdog-models:mongrel2-handler)
                   (server-targets server))))

(defmethod mongrel2-handler-name ((handler fdog-models:mongrel2-handler))
  "Return the name, or part before the subscription UUID and the UUID as
a multivalue return as in (values name uuid)"
  (destructuring-bind (name id) (ppcre:split "--" (fdog-models:mongrel2-handler-recv-ident handler) :limit 2)
    (values name id)))
