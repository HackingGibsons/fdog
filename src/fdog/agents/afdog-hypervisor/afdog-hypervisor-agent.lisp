(in-package :afdog-hypervisor-agent)

(defclass afdog-hypervisor-agent (standard-hypervisor-agent rooted-agent-mixin)
  ((agents :accessor hypervisor-agents :initarg :agents)))

(defgeneric link-agent (agent organ root)
  (:documentation "Link `agent' by sending the link command to `organ'."))

(defgeneric link-agents (agent organ &key)
  (:documentation "Link child agents `agent' is aware of by sending the link commands to `organ'."))

(defmethod link-agent ((agent standard-agent) root organ)
  (let ((command `(:command :link
                            :link :agent
                            :agent `(:uuid ,(agent-uuid agent)
                                           :class ,(class-name (class-of agent))
                                           :root ,root))))
    (send-message organ :command command)))

(defmethod link-agents ((agent afdog-hypervisor-agent) organ &key)
  (log-for (trace afdog-hypervisor-agent) "Linking agents for agent ~A" agent)
  (mapcar (lambda (child) 
            (link-agent child organ (agent-root agent)))
          (hypervisor-agents agent)))

(defmethod agent-special-event :after ((agent afdog-hypervisor-agent) (event-head (eql :boot)) event)
  "On boot the afdog-hypervisor agent will link all agents the hypervisor knows about."
  (let ((head (find-organ agent :head)))
    (link-agents agent head)))
