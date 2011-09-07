(in-package :agent)

(defclass agent-head (standard-beating-organ)
  ((organs :initform (make-hash-table :test 'equalp)
           :accessor agent-organs))

  (:documentation "This organ is responsible for decision making and task scheduling, but none of the work.")
  (:default-initargs . (:tag :head)))

(defmethod act-on-event ((head agent-head) event)
  (prog1 event
    (cond ((and (listp event) (getf (reverse event) :beat))
           (organ-beat-event head event))
          (:otherwise
           (log-for (warn) "~A doesn't deal with ~A" head event)))))

(defmethod organ-beat-event ((head agent-head) (event list))
  "Handle the updating of an organ's state."
  (let ((tag (car event))
        (uuid (getf event :uuid))
        (time (getf event :time)))

    (unless (and tag uuid time)
      (log-for (warn) "~A: organ-beat-event did nat have all of: tag, :uuid or :time" head)
      (error "organ-beat-event did nat have all of: tag, :uuid or :time"))

    (log-for (trace) "Updating organ: ~A/~A @ ~A" tag uuid time)))
