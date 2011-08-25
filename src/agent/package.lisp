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
   (uuid :initarg :uuid
         :reader agent-uuid
         :initform (print-object (uuid:make-v4-uuid) nil)))

  (:documentation "A standard agent shell. Capable of communication, but completely dead inside."))

(defclass vertebrate-agent (standard-agent)
  ((spine :initform (make-hash-table :test #'equal)
          :reader agent-spine
          :documentation "A hash table of nerve junctions")))

;; Pretty-printers
(defmethod print-object ((agent standard-agent) s)
  (format s "#<Agent[~A] Running: ~A" (agent-uuid agent) (agent-running-p agent)))

(defmethod print-object ((agent vertebrate-agent) s)
  (format s "#<VertebrateAgent[~A][~A] Running: ~A" (agent-uuid agent)
          (hash-table-count (agent-spine agent))
          (agent-running-p agent)))

;; Generics/Base methods
(defgeneric agent-running-p (agent)
  (:documentation "Generic predicate for agent running test. Tests for context existance.")
  (:method ((agent standard-agent))
    (not (not (agent-context agent)))))

(defgeneric start-agent (agent &rest options &key &allow-other-keys)
  (:documentation "Start the agent `agent'.")
  (:method ((agent standard-agent) &rest options)
    (declare (ignorable options))
    (log-for (trace) "Creating agent context(~A) for ~A" *context-threads* agent)
    (setf (slot-value agent 'context) (zmq:init *context-threads*))
    agent))

(defgeneric stop-agent (agent)
  (:documentation "Stop the agent `agent'")
  (:method ((agent standard-agent))
    (log-for (trace) "Terminating context of: ~A" agent)
    (zmq:term (agent-context agent))
    (log-for (trace) "Terminated context of: ~A" agent)
    (setf (slot-value agent 'context) nil)
    agent))
