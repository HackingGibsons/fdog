(in-package :mongrel2-agent)

;; TODO: Fold into the agents dump
(defclass rooted-agent-mixin ()
  ((root :initarg :root :initform *root*
         :accessor agent-root)))

;; Agent
(defclass mongrel2-agent (standard-hypervisor-agent rooted-agent-mixin)
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
               (clsql:update-records-from-instance route)
               route))
           (make-default-host (server)
             (let ((host (fdog-models:make-host "localhost")))
               (setf (fdog-models:mongrel2-host-server-id host) (fdog-models:mongrel2-server-id server))
               (clsql:update-records-from-instance host)
               host))
           (make-default-server ()
             (fdog-models:make-server "control" :chroot (namestring server-root)))
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
(defmethod agent-info ((agent mongrel2-agent))
  "Produce a `mongrel2-agent' specific agent announcement."
  (let* ((info (call-next-method))
         (servers (fdog-models:servers :refresh t)))
    (flet ((server-ad (server)
             (fdog-models:mongrel2-server-name server)))

      (append info `(:provides (:servers ,(mapcar #'server-ad servers)))))))


(defmethod agent-special-event :after ((agent mongrel2-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (labels ((make-mongrel2-arguments (server config)
             (let ((uuid (fdog-models:mongrel2-server-uuid server)))
               `(:path "/usr/bin/env" :args ("mongrel2" ,(namestring config) ,uuid))))
           (link-server (organ server config)
             (let ((arguments (make-mongrel2-arguments server config))
                   (pid (fdog-models:mongrel2-server-pid server)))
               (log-for (mongrel2-agent trace) "Found mongrel2 server ~A to link with make arguments of ~A." server arguments)
               (send-message organ :command `(:command :link
                                                       :link :process
                                                       :process (:pid ,pid ,@arguments))))))
    (let* ((head (find-organ agent :head))
           (server-root (ensure-mongrel2-root-layout (agent-root agent)))
           (config (merge-pathnames fdog-models:*config-file*
                                    server-root))
           tables)
      (log-for (mongrel2-agent trace) "Root path: ~A" config)
      (fdog-models:connect config)
      (setf tables (clsql:list-tables))
      (log-for (mongrel2-agent trace) "Found tables ~{~A~^ ~}" tables)
      (unless (find "server" tables :test #'string-equal)
        (log5:log-for (trace mongrel2-agent) "Setting up a default configuration.")
        (initialize-mongrel2-configuration server-root))
      (mapc #'(lambda (server)
                (log-for (trace mongrel2-agent) "Linking server ~A" server)
                (link-server head server config))
            (fdog-models:servers)))))
