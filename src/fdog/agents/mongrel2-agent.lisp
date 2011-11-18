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

(defmethod agent-special-event :after ((agent mongrel2-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let ((head (find-organ agent :head))
        (config (merge-pathnames (make-pathname :directory '(:relative "server") :name "config" :type "sqlite")
                                 *root*)))

    (log-for (mongrel2-agent trace) "Root path: ~A" config (probe-file config))
    (ensure-directories-exist config :verbose t)

    (if (probe-file config)
        (log-for (mongrel2-agent warn) "Config exists.")
        (log-for (mongrel2-agent warn) "Config missing"))))
