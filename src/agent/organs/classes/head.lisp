(in-package :agent)

(defclass agent-head (standard-organ)
  ()
  (:documentation "This organ is responsible for decision making and task scheduling, but none of the work."))

(defmethod act-on-event ((organ agent-head) event)
  (log-for (warn) "THIS HEAD DOES NOTHING: [~A]::~A" (type-of event) event))
