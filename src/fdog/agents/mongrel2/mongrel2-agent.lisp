(in-package :mongrel2-agent)

(defvar *control-port* 1337
  "The HTTP Port for the control server.")

;; Agent
(defclass mongrel2-agent (standard-manager-agent rooted-agent-mixin)
  ()
  (:documentation "Mongrel2 Agent."))

;; Helpers
(defun initialize-mongrel2-configuration (server-root)
  (labels ((make-default-static-dir ()
             (fdog-models:make-dir "./public/"))
           (make-default-route (host)
             (let* ((target (make-default-static-dir))
                    (route  (fdog-models:make-route "/static/" target)))
               (setf (fdog-models:mongrel2-route-host-id route) (fdog-models:mongrel2-host-id host))
               (with-clsql-retry ()
                 (clsql:update-records-from-instance route))
               route))
           (make-default-host (server)
             (let ((host (fdog-models:make-host "localhost")))
               (setf (fdog-models:mongrel2-host-server-id host) (fdog-models:mongrel2-server-id server))
               (with-clsql-retry ()
                 (clsql:update-records-from-instance host))
               host))
           (make-default-server ()
             (fdog-models:make-server "control" :port *control-port* :chroot (namestring server-root)))
           (make-default-configuration ()
             (let* ((server (make-default-server))
                    (host (make-default-host server))
                    (route (make-default-route host)))
               (values server host route))))
    (log-for (trace mongrel2-agent) "Intializing sever layout.")
    (fdog-models:init)
    (log-for (trace mongrel2-agent) "Installing default configuration.")
    (make-default-configuration)))

(defun ensure-mongrel2-root-layout (root)
  (let* ((server-root (merge-pathnames (make-pathname :directory fdog-models:*server-dir*)
                                       root))
         (logs (merge-pathnames (make-pathname :directory '(:relative "logs"))
                                server-root))
         (tmp (merge-pathnames (make-pathname :directory '(:relative "tmp"))
                               server-root))
         (run (merge-pathnames (make-pathname :directory '(:relative "run"))
                               server-root)))

    (mapc #'(lambda (p)
              (log-for (mongrel2-agent trace) "Ensuring ~A exists" p)
              (ensure-directories-exist p :verbose t)) (list server-root logs run tmp))
    server-root))

;; Hooks
(defmethod agent-provides :around ((agent mongrel2-agent))
  (labels ((handler-description (handler)
             (let ((name (mongrel2-handler-name handler)))
               `(,name . (:send ,(fdog-models:mongrel2-handler-send-spec handler)
                          :recv ,(fdog-models:mongrel2-handler-recv-spec handler)))))

           (server-ad (server)
             `(,(fdog-models:mongrel2-server-name server)
                .
               ,(loop for handler in (mongrel2-server-handlers server)
                   collecting (handler-description handler)))))

    (append (call-next-method)
            `(:servers ,(mapcar #'server-ad (fdog-models:servers :refresh t))))))

(defmethod agent-special-event :after ((agent mongrel2-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let* ((head (find-organ agent :head))
         (server-root (ensure-mongrel2-root-layout (agent-root agent)))
         (config (merge-pathnames fdog-models:*config-file*
                                  server-root))
         tables)

    (ignore-errors (fdog-models:disconnect))
    (ignore-errors (clsql:disconnect))
    (log-for (mongrel2-agent trace) "Root path: ~A" config)
    (fdog-models:connect config)

    (setf tables (clsql:list-tables))
    (log-for (mongrel2-agent trace) "Found tables ~{~A~^ ~}" tables)
    (unless (find "server" tables :test #'string-equal)
      (log5:log-for (trace mongrel2-agent) "Setting up a default configuration.")
      (initialize-mongrel2-configuration server-root))

    (link-all-servers head config)))
