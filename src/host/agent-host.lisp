(in-package :agent-host)

;; Class and init
(defclass agent-host ()
  ((context :initarg :context
            :initform nil
            :accessor context)
   (added :initarg :agents
          :initarg :add
          :initform (list)
          :accessor added)
   (removed :initform (list)
            :accessor removed
            :documentation "List of UUIDs of agents that should be removed")
   (agents :initform nil
           :accessor agents)

   (running :initform nil
            :accessor running
            :reader running-p)
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
