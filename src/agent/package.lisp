(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog))

(in-package :agent)

;; Globals
(defparameter *context-threads* 1)

;; Classes
(defclass standard-agent ()
  ((uuid :initarg :uuid
         :reader agent-uuid
         :initform (print-object (uuid:make-v4-uuid) nil))

   (context :initarg :context
            :reader agent-context
            :initform nil)
   (event-addr :initarg :event-addr
               :initform (format nil "tcp://127.0.0.1:30308")
               :reader agent-event-addr)
   (event-sock :reader agent-event-sock)


   (event-count :initform 0
                :accessor agent-event-count)
   (last-tick :initform 0
              :accessor agent-last-tick)
   (tick-delta :initform 0
               :accessor agent-tick-delta))

  (:documentation "A standard agent shell. Capable of communication, but completely dead inside."))

(defun make-agent ()
  (make-instance 'standard-agent))

(defmethod next-event ((agent standard-agent))
  (log-for (trace) "Fetching event for: ~A" agent)
  (flet ((s2us (s) (round (* s 1000000)))

         (read-message ()
           (let ((msg (make-instance 'zmq:msg)))
             (zmq:recv! (agent-event-sock agent) msg)
             (zmq:msg-data-as-string msg))))
    (zmq:with-polls ((readers . (((agent-event-sock agent) . zmq:pollin))))
      (if (zmq:poll readers :timeout (s2us 1))
          (read-message)
          :timeout))))


(defmethod event-fatal-p ((agent standard-agent) event)
  (log-for (trace) "Testing event fatalaty of ~A for ~A" event agent)
  (not event))

(defmethod run-agent ((agent standard-agent))
  (zmq:with-context (ctx *context-threads*)
    (zmq:with-socket (event-sock ctx zmq:sub)
      (log-for (trace) "Binding event sock to: ~A" (agent-event-addr agent))
      (zmq:bind event-sock (agent-event-addr agent))

      (log-for (trace) "Setting agent context and event sock: ~A" agent)
      (setf (slot-value agent 'context) ctx
            (slot-value agent 'event-sock) event-sock)


      ;; Agent event loop
      (log-for (trace) "Entering agent event loop.")
      (do ((event (next-event agent) (next-event agent)))
          ((or (not event)
               (event-fatal-p agent event))
           event)
        (log-for (trace) "Agent[~A] Event: ~A" agent event)

        ;; Tick, Process, Increment counter
        (agent-tick agent)
        (act-on-event agent event)
        (incf (agent-event-count agent)))

      (log-for (trace) "Agent exiting: ~A. ~A events processed" agent (agent-event-count agent)))))

(defmethod act-on-event ((agent standard-agent) event)
  (log-for (trace) "Agent: ~A processing event: ~A" agent event))

(defmethod agent-tick ((agent standard-agent))
  "Internal tick, measure time queue timed events."
  (let ((last (agent-last-tick agent))
        (now (get-internal-real-time)))
    (setf (agent-tick-delta agent) (- now last)
          (agent-last-tick agent) now)
    (log-for (trace) "Agent ~A tick. Tick Delta: ~A" agent (agent-tick-delta agent))))
