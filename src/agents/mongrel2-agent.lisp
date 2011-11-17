(in-package :mongrel2-agent)
(defcategory mongrel2-state-machine)

(defclass mongrel2-state-machine (standard-state-machine)
  ((behavior :initform nil :initarg :behavior
             :reader behavior))
  (:documentation "State machine to search for mongrel2 servers to supervise.")
  (:metaclass c2mop:funcallable-standard-class))
