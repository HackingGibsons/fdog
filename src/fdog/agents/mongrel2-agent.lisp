(in-package :mongrel2-agent)

;; TODO: Fold into the agents dump
(defclass rooted-agent-mixin ()
  ((root :initarg :root :initform *root*
         :accessor agent-root)))

;; Agent
(defcategory mongrel2-agent)
(defclass mongrel2-agent (standard-hypervisor-agent rooted-agent-mixin)
  ()
  (:documentation "Mongrel2 Agent."))

(defun initialize-mongrel2-configuration (server-root config)
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
    (log5:log-for (trace mongrel2-agent) "Intializing ~%")
    (fdog-models:init)
    (log5:log-for (trace mongrel2-agent) "Installing default configuration.~%")
    (make-default-configuration)))

(defun ensure-mongrel2-root-layout (root)
  (let* ((server-root (merge-pathnames (make-pathname :directory fdog-models:*server-dir*)
                                       root))
         (config (merge-pathnames fdog-models:*config-file*
                                  server-root))
         (logs (merge-pathnames (make-pathname :directory '(:relative "logs"))
                                server-root))
         (tmp (merge-pathnames (make-pathname :directory '(:relative "tmp"))
                               server-root))
         (run (merge-pathnames (make-pathname :directory '(:relative "run"))
                               server-root)))

          (log-for (mongrel2-agent trace) "Root path: ~A" config (probe-file config))
          (mapc #'(lambda (p)
                    (ensure-directories-exist p :verbose t)) (list config logs run tmp))
          (values server-root config)))

(defmethod agent-special-event :after ((agent mongrel2-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let ((head (find-organ agent :head)))
    (labels ((make-mongrel2-arguments (server config)
               (let ((uuid (fdog-models:mongrel2-server-uuid server)))
                 `(:path "/usr/bin/env" :args ("mongrel2" ,(namestring config) ,uuid))))
             (link-server (server config)
               (let ((arguments (make-mongrel2-arguments server config))
                     (pid (fdog-models:mongrel2-server-pid server)))
                 (log-for (mongrel2-agent trace) "Found mongrel2 server ~A to link with make arguments of ~A." server arguments)
                 (send-message head :command `(:command :link
                                                        :link :process
                                                        :process (:pid ,pid
                                                                       :make ,arguments))))))

      (multiple-value-bind (server-root config) (ensure-mongrel2-root-layout *root*)
        (fdog-models:connect)
        (let ((tables (clsql:list-tables)))
          (unless (find "SERVER" tables :test #'string-equal)
            (log5:log-for (trace mongrel2-agent) "Setting up a default configuration.")
            (initialize-mongrel2-configuration server-root config)))

        (mapc #'(lambda (server)
                  (link-server server config)) (fdog-models:servers))))))

