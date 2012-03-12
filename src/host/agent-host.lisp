(in-package :agent-host)

;; Class and init
(defclass agent-host ()
  ((context :initarg :context
            :initform nil
            :accessor context
            :documentation "The ZeroMQ context that this host will attach to the agents.
It's finalized with the host instance, should not need explicit termination.")

   ;; Agent management slots
   (added :initarg :agents
          :initarg :add
          :initform (list)
          :accessor added
          :documentation "Agents that have been added to the host but not yet registered.
Agent registration will transfer the agent instance to the `agents' list after connecting
all of its sockets.")
   (removed :initform (list)
            :accessor removed
            :documentation "List of UUIDs of agents that should be removed at the end of a tick.")
   (agents :initform nil
           :accessor agents
           :documentation "List of agent instances that the host is currently actively managing.")

   ;; Info and statistics
   (running :initform nil
            :accessor running
            :reader running-p
            :documentation "Flag to signal the state of the host.")
   (ticks :initform 0
          :accessor ticks
          :documentation "Number of iterations of the event loop.")
   (events :initform 0
           :accessor events
           :documentation "Number of events fired from the event loop."))
  (:documentation "A container for running multiple agents in a single event
loop/process."))

(defmethod initialize-instance :after ((inst agent-host) &key)
  "Setup the context and finalizer."
  (let ((ctx (zmq:init 1)))
    (tg:finalize inst #'(lambda () (ignore-errors (zmq:term ctx))))
    (setf (context inst) ctx)))
