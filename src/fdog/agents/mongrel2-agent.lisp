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

(defun initialize-mongrel2-configuration ()
  (fdog-models:init)
  (fdog-models:make-server "control"))

(defmethod agent-special-event :after ((agent mongrel2-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let ((head (find-organ agent :head))
        (config (merge-pathnames (make-pathname :directory '(:relative "server") :name "config" :type "sqlite")
                                 *root*)))
    (labels ((make-mongrel2-arguments (server)
               (let ((uuid (fdog-models:mongrel2-server-uuid server)))
                 `(:path "mongrel2" :args ,(list (namestring config) uuid))))
             (link-server (server)
               (let ((arguments (make-mongrel2-arguments server))
                     (pid (fdog-models:mongrel2-server-pid server)))
                 (log-for (mongrel2-agent trace) "Found mongrel2 server ~A to link with make arguments of ~A." server arguments)
                 (format t "Found mongrel2 server ~A to link with make arguments of ~A.~%" server arguments)
                 (send-message head :command `(:command :link
                                                        :link :process
                                                        :process (:pid ,pid
                                                                       :make ,arguments))))))

      (log-for (mongrel2-agent trace) "Root path: ~A" config (probe-file config))
      (ensure-directories-exist config :verbose t)

      (fdog-models:connect)
      (let ((tables (clsql:list-tables)))
        (unless (find "SERVER" tables :test #'string-equal)
          (initialize-mongrel2-configuration)))

      (mapc #'link-server (fdog-models:servers)))))

