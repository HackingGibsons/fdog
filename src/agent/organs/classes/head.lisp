(in-package :agent)

(defclass agent-head (standard-beating-organ)
  ((organs :initform (make-hash-table :test 'equalp)
           :accessor agent-organs))

  (:documentation "This organ is responsible for decision making and task scheduling, but none of the work.")
  (:default-initargs . (:tag :head)))

