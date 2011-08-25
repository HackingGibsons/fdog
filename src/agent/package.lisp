(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog))

(in-package :agent)

;; Globals
(defparameter *context-threads* 1)

;; Classes
(defclass standard-agent ()
  ((context :initarg :context
            :reader agent-context
            :initform nil)
   (event-count :initform 0
                :accessor agent-event-count)
   (last-tick :initform 0
              :accessor agent-last-tick)
   (tick-delta :initform 0
               :accessor agent-tick-delta)
   (uuid :initarg :uuid
         :reader agent-uuid
         :initform (print-object (uuid:make-v4-uuid) nil)))

  (:documentation "A standard agent shell. Capable of communication, but completely dead inside."))

(defun make-agent ()
  (make-instance 'standard-agent))

(defmethod next-event ((agent standard-agent))
  (log-for (trace) "Fetching event for: ~A" agent)
  (let ((e (random 100)))
    (if (= e 0) nil e)))

(defmethod event-fatal-p ((agent standard-agent) event)
  (log-for (trace) "Testing event fatalaty of ~A for ~A" event agent)
  (not event))

(defmethod run-agent ((agent standard-agent))
  (zmq:with-context (ctx *context-threads*)
    (log-for (trace) "Setting agent context: ~A" agent)
    (setf (slot-value agent 'context) ctx)

    ;; Agent event loop
    (do ((event (next-event agent) (next-event agent)))
        ((or (not event)
             (event-fatal-p agent event))
         event)
      (log-for (trace) "Agent[~A] Event: ~A" agent event)
      ;; Tick, Process, Increment counter
      (agent-tick agent)
      (act-on-event agent event)
      (incf (agent-event-count agent)))

    (log-for (trace) "Agent exiting: ~A. ~A events processed" agent (agent-event-count agent))))

(defmethod act-on-event ((agent standard-agent) event)
  (log-for (trace) "Agent: ~A processing event: ~A" agent event))

(defmethod agent-tick ((agent standard-agent))
  "Internal tick, measure time queue timed events."
  (let ((last (agent-last-tick agent))
        (now (get-internal-real-time)))
    (setf (agent-tick-delta agent) (- now last)
          (agent-last-tick agent) now)
    (log-for (trace) "Agent ~A tick. Tick Delta: ~A" agent (agent-tick-delta agent))))
