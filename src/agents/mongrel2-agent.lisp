(in-package :mongrel2-agent)

;; TODO: Fold into the agents dump
(defclass rooted-agent-mixin ()
  ((root :initarg :root :initform nil
         :accessor agent-root)))

;; Agent
(defclass mongrel2-agent (standard-hypervisor-agent rooted-agent-mixin)
  ()
  (:documentation "Mongrel2 Agent."))

(defmethod agent-special-event :after ((agent mongrel2-agent) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let ((head (find-organ agent :head)))
    (make-manage-mongrels head)))

;; FSM
(defcategory mongrel2-fsm)
(defclass mongrel2-state-machine (standard-state-machine)
  ((behavior :initarg :behavior
             :accessor behavior))
  (:documentation "State machine to search for mongrel2 servers to supervise.")
  (:metaclass c2mop:funcallable-standard-class))

(defstate mongrel2-state-machine :initial (event)
  (log-for (trace mongrel2-fsm)  ":initial FSM Event: ~A~%" event))

;; Behavior
(defclass mongrel2-manager ()
  ((state :accessor state)))

(defmethod initialize-instance :after ((inst mongrel2-manager) &key)
  (setf (state inst) (make-instance 'mongrel2-state-machine :behavior inst)))


(defbehavior manage-mongrels (:on (:fizz :buzz :from :head) :do :invoke-with-event
                                  :include (mongrel2-manager)) (organ event)

  (funcall (state behavior) event))
