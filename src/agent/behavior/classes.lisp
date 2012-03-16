(in-package :agent)

(defclass behaving-organ-mixin ()
  ((behaviors :initarg :behaviors :initform nil
              :accessor behaviors))
  (:documentation "Mixed into organ classes when a behavior is attached to them."))

(defclass standard-behavior (c2mop:funcallable-standard-object)
  ((organ :initarg :organ :initform nil
          :accessor behavior-organ)
   (invoke-when :initarg :invoke-when
                :accessor invoke-when)
   (invoke-p :accessor invoke-p))

  (:documentation "The base class of any behaviors created."))

(defclass interval-behavior ()
  ((recent-events :initform nil
                  :initarg :recent-events :accessor recent-events)
   (last-invoked :initform nil
                 :initarg :last-invoked :accessor last-invoked))

  (:documentation "Mixin class for events that head their behavior with :interval"))

(defmethod behavior-compile-invoke-p (behavior description)
  "Compile the `description' of when a `behavior' shoud be invoked based on the
declared rules inside the definition of the `behavior' to a predicate taking an event.

The resulting predicate lambda will be checked in `act-on-event' specialization for any organ that gets a
behavior attached to predicate the invocation of the funcallabable lambda of that behavior."
  (eval
   `(let ((behavior ,behavior))
      (declare (ignorable behavior))
      (macrolet ((:or (definitions &key &allow-other-keys)
                   (let* ((predicates (mapcar #'(lambda (def) (behavior-compile-invoke-p ,behavior def))
                                             definitions)))
                     (flet ((test-all (e)
                              (some #'(lambda (f) (funcall f e))
                                    predicates)))
                       `(lambda (event)
                          (funcall ,#'test-all event)))))

                 (:on (definition &key &allow-other-keys)
                   `(let ((from (getf ',definition :from))
                          (type (car ',definition))
                          (message (second ',definition)))
                      (lambda (event)
                        (let ((event-from (car event))
                              (event-type (cadr event)))
                          (when (and (eql event-from from)
                                     (eql event-type type)
                                     (eql (getf event type) message))
                            t)))))

                 (:interval (definition &key &allow-other-keys)
                   `(let ((from (getf ',definition :from))
                          (nth (getf ',definition :nth)))
                      (lambda (event)
                        (when (eql (and (consp event) (car event)) from)
                          (let ((time (getf event :time)))
                            (and time
                                 (setf (recent-events behavior)
                                       (subseq (push time (recent-events behavior))
                                               0 (min nth (length (recent-events behavior))))))

                            (if (not (find (last-invoked behavior) (recent-events behavior)))
                                (prog1 t
                                  (setf (last-invoked behavior) time))
                                nil)))))))
        ,description))))

(defmethod initialize-instance :after ((behavior standard-behavior) &key)
  "Compile the invocation event predicate `invoke-p' for the given `behavior' and store within in a slot"
  (log-for (trace) "Computing `invoke-p' from `invoke-when' for ~A" behavior)
  (setf (invoke-p behavior) (behavior-compile-invoke-p behavior (invoke-when behavior))))

(defmethod act-on-event :after ((organ behaving-organ-mixin) event)
  "Deterimine if this event should result in a behavior invocation"
  (let (fired)
    (dolist (behavior (behaviors organ) fired)
      (when (funcall (invoke-p behavior) event)
        (case (getf (invoke-when behavior) :do)
          (:invoke (funcall behavior organ))
          (:invoke-with-event (funcall behavior organ event)))
        (push behavior fired )))))
