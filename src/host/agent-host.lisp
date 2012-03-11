(in-package :agent-host)

;; Class and init
(defclass agent-host ()
  ((context :initarg :context
            :initform nil
            :accessor context)
   (agents :initarg :agents
           :initform nil
           :accessor agents)

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
