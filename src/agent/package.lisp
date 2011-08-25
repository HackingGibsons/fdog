(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog))

(in-package :agent)

;; Globals
(defparameter *context-threads* 1
  "Number of threads an agent context uses.")

;; Classes
(defclass standard-agent ()
  ((uuid :initarg :uuid
         :reader agent-uuid
         :initform (print-object (uuid:make-v4-uuid) nil))

   (context :initarg :context
            :reader agent-context
            :initform nil)
   ;; Subscription addr/sock pair
   (event-addr :initarg :event-addr
               :reader agent-event-addr)
   (event-sock :reader agent-event-sock)
   ;; Publish addr/sock pair
   (message-addr :initarg :message-addr
                 :reader agent-message-addr)
   (message-sock :reader agent-message-sock)


   (event-count :initform 0
                :accessor agent-event-count)
   (last-event :initform (get-internal-real-time)
               :accessor agent-last-event)
   (last-tick :initform 0
              :accessor agent-last-tick)
   (tick-delta :initform 0
               :accessor agent-tick-delta))

  (:documentation "A standard agent shell. Capable of communication, but completely dead inside."))

(defmethod initialize-instance :after ((agent standard-agent) &rest initargs)
  "Set up the interal bus address."
  (declare (ignorable initargs))
  (log-for (trace) "Setting the event-addr to inproc://uuid")
  (setf (slot-value agent 'event-addr)
        (format nil "inproc://~A" (agent-uuid agent)))
  (log-for (trace) "Setting the event-addr to inproc://msg-uuid")
  (setf (slot-value agent 'message-addr)
        (format nil "inproc://msg-~A" (agent-uuid agent))))

(defun make-agent ()
  "Agent maker wrapper"
  (make-instance 'standard-agent))

(defmethod next-event ((agent standard-agent))
  "Returns the next event pending for `agent' on the internal bus
or `:timeout' if no event is found after a pause."
  (log-for (trace) "Fetching event for: ~A" agent)
  (flet ((s2us (s) (round (* s 1000000)))

         (read-message ()
           (let ((msg (make-instance 'zmq:msg)))
             (zmq:recv! (agent-event-sock agent) msg)
             (zmq:msg-data-as-string msg))))
    (zmq:with-polls ((readers . (((agent-event-sock agent) . zmq:pollin))))
      (if (zmq:poll readers :timeout (s2us 1) :retry t)
          (read-message)
          :timeout))))


(defmethod event-fatal-p ((agent standard-agent) event)
  "Predicate to determine if this event should end the agent."
  (log-for (trace) "Testing event fatalaty of ~A for ~A" event agent)
  (let ((timeout 3))
    (if (and (event-timeout-p event)
             (> (- (get-internal-real-time) (agent-last-event agent))
                (* timeout internal-time-units-per-second)))
        (prog1 t (log-for (warn) "Event timeout ~As reached." timeout))
        (not event))))

(defmethod run-agent ((agent standard-agent))
  "Enter the agent event loop, return only when agent is dead."
  (zmq:with-context (ctx *context-threads*)
    (zmq:with-socket (event-sock ctx zmq:sub)
      (zmq:with-socket (message-sock ctx zmq:pub)
        (log-for (trace) "Binding event sock to: ~A" (agent-event-addr agent))
        (zmq:bind event-sock (agent-event-addr agent))
        (log-for (warn) "Subscribing event sock to everyting")
        (zmq:setsockopt event-sock zmq:subscribe "")

        (log-for (trace) "Binding message sock to: ~A" (agent-message-addr agent))
        (zmq:bind message-sock (agent-message-addr agent))

        (log-for (trace) "Setting agent context and event sock: ~A" agent)
        (setf (slot-value agent 'context) ctx
              (slot-value agent 'event-sock) event-sock
              (slot-value agent 'message-sock) message-sock)


        ;; Agent event loop
        (setf (agent-last-event agent) (get-internal-real-time))
        (setf (agent-event-count agent) 0)
        (log-for (trace) "Entering agent event loop.")
        (do ((event (next-event agent) (next-event agent)))
            ((or (not event)
                 (event-fatal-p agent event))
             event)
          (log-for (trace) "Agent[~A] Event: ~A" agent event)

          ;; Tick, Process, Increment counter
          (agent-tick agent event)
          (act-on-event agent event)
          (incf (agent-event-count agent)))

        (log-for (trace) "Agent exiting: ~A. ~A events processed" agent (agent-event-count agent))))))

(defgeneric prepare-message (message)
  (:method (message)
    (make-instance 'zmq:msg :data (with-output-to-string (s) (prin1 message s))))
  (:method ((message string))
    (make-instance 'zmq:msg :data message)))

(defgeneric agent-send-message (agent event)
  (:method ((agent standard-agent) event)
    (log-for (trace) "Sending message: [~A]" (with-output-to-string (s) (prin1 event s)))
    (zmq:send! (agent-message-sock agent) (prepare-message event))))

(defmethod act-on-event ((agent standard-agent) event)
  "Perform any action an `agent' would need to take to act on `event'"
  (log-for (trace) "Agent: ~A processing event: ~A" agent event)

  ;; Rebroadcast useful events
  (unless (or (not event) (event-timeout-p event))
    (agent-send-message agent event)))

(defun event-timeout-p (event)
  "Predicate testing if the `event' represents a timeout event"
  (equal :timeout event))

(defmethod agent-tick ((agent standard-agent) event)
  "Internal tick, measure time, queue timed events, update timeout clock."
  (let ((last (agent-last-tick agent))
        (now (get-internal-real-time)))

    (unless (event-timeout-p event)
      (log-for (trace) "Not a timeout event.")
      (setf (agent-last-event agent) now))

    (setf (agent-tick-delta agent) (- now last)
          (agent-last-tick agent) now)

    (log-for (trace) "Agent ~A tick. Tick Delta: ~A" agent (agent-tick-delta agent))))
