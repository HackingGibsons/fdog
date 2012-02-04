(in-package :afdog-hypervisor-agent)

(defclass afdog-hypervisor-agent (standard-hypervisor-agent rooted-agent-mixin)
  ((agents :accessor hypervisor-agents :initarg :agents
           :initform '(mongrel2-agent ()
                       api-agent ())
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
         (uuid (or (getf args :uuid)  
                   (format nil "~A" (uuid:make-v4-uuid))))
         (args (or (when (remf args :uuid)
                     args)
                   args))
         (command `(:command :link
                             :link :agent
                             :agent (:uuid ,uuid
                                     :parent-uuid ,parent-uuid
                                     :parent-mouth ,parent-mouth
                                     :class ,name
                                     :root ,root
                                     :package ,(intern (package-name (symbol-package name)) :keyword)
                                     ,@args))))
    (log-for (afdog-hypervisor-agent trace) "Command ~A" command)
    (send-message organ :command command)))

(defmethod link-all-agents ((agent afdog-hypervisor-agent))
  (let ((root (agent-root agent))
        (organ (find-organ agent :head)))
    (loop for (name args) on (hypervisor-agents agent) by #'cddr do
         (log-for (afdog-hypervisor-agent trace) "Linking agent: ~A with args ~S at root ~S" name args root)
         (link-agent organ name root args))))

(defmethod agent-special-event :after ((agent afdog-hypervisor-agent) (event-head (eql :boot)) event)
  "On boot the afdog-hypervisor agent will link all agents the hypervisor knows about."
  (log-for (afdog-hypervisor-agent trace) "Hypervisor is booting. With agents:~S" (hypervisor-agents agent))
  (link-all-agents agent))
