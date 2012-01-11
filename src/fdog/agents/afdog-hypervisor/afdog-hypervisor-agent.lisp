(in-package :afdog-hypervisor-agent)

(defclass afdog-hypervisor-agent (standard-hypervisor-agent rooted-agent-mixin)
  ((agents :accessor hypervisor-agents :initarg :agents
           :initform '(mongrel2-agent ())
           :documentation "A plist of symbols and lists.
The symbol should be the class name of an agent.
The list should be a list of initargs.")))

(defgeneric link-agent (organ agent-name root args)
  (:documentation "Link an agent."))

(defgeneric link-all-agents (agent)
  (:documentation "Call link-agent for agents of `agent'."))

(defmethod link-agent ((organ standard-organ) name root args)
  (let* ((parent-uuid (agent-uuid (organ-agent organ)))
         (parent-mouth (mouth-addr (find-organ (organ-agent organ) :mouth)))
         (command `(:command :link
                             :link :agent
                             :agent (:uuid ,(format nil "~A" (uuid:make-v4-uuid))
                                     :parent-uuid ,parent-uuid
                                     :parent-mouth ,parent-mouth
                                     :class ,name
                                     :root ,root
                                     :package ,(intern (package-name (symbol-package name)) :keyword)
                                     ,@args))))
    (send-message organ :command command)))

(defmethod link-all-agents ((agent afdog-hypervisor-agent))
  (let ((root (agent-root agent))
        (organ (find-organ agent :head)))
    (loop for (name args) on (hypervisor-agents agent) by #'cddr do
         (log-for (afdog-hypervisor-agent) "Linking agent: ~A with args ~S" name args)
         (link-agent organ name root args))))

(defmethod agent-special-event :after ((agent afdog-hypervisor-agent) (event-head (eql :boot)) event)
  "On boot the afdog-hypervisor agent will link all agents the hypervisor knows about."
  (link-all-agents agent))
