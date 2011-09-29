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
      (macrolet ((:interval (definition verb noun)
                   `(let ((from (getf ',definition :from))
                          (nth (getf ',definition :nth)))
                      (lambda (event)
                        (log-for (warn) "TODO: Compiled ~A checker for every ~Ath beat from ~A" behavior nth from)
                        (log-for (warn) "Current event: ~A" event)
                        (log-for (warn) "Typed: ~A" (type-of event))
                        (when (eql (and (consp event) (car event)) from)
                          (let ((time (getf event :time)))
                            (and time
                                 (setf (recent-events behavior)
                                       (subseq (push time (recent-events behavior))
                                               0 (min nth (length (recent-events behavior))))))

                            (log-for (warn) "TODO: Events in ~A to date: ~A" behavior (recent-events behavior))
                            (log-for (warn) "Seeking: ~A" (last-invoked behavior))

                            (if (not (find (last-invoked behavior) (recent-events behavior)))
                                (prog1 t
                                  (log-for (trace) "Seems we haven't been invoked in a while. Let's")
                                  (setf (last-invoked behavior) time))
                                nil)))))))
        ,description))))

(defmethod initialize-instance :after ((behavior standard-behavior) &key)
  "Compile the invocation event predicate `invoke-p' for the given `behavior' and store within in a slot"
  (log-for (trace) "Computing `invoke-p' from `invoke-when' for ~A" behavior)
  (setf (invoke-p behavior) (behavior-compile-invoke-p behavior (invoke-when behavior))))

(defmacro defbehavior (name behavior invoke-lambda &body body)
  "Generates a behavior class and and makes it funcallable with `invoke-lambda' as the lambda
list, evaluating `body' on invokation as governed by the `behavior' list. The scope will
contain `behavior' bound to the current instance of the behavior class `name'"
  `(progn
     (defclass ,name (standard-behavior ,(cond ((eql (car behavior) :interval) 'interval-behavior)
                                               (t (values))))
       ()
       (:metaclass c2mop:funcallable-standard-class))

     (defmethod ,(intern (string-upcase (concatenate 'string "make-" (symbol-name name)))) (organ &key)
       "Make an instance of a behavior class, updating `organ' to include the `behaving-organ-mixin'
and attaching this behavior to the existing list."
       (log-for (trace) "Making ~A" (symbol-name ',name))

       (let* ((new-organ-class (make-instance 'standard-class :name (type-of organ)
                                              :direct-superclasses `(,(class-of organ) ,(find-class 'behaving-organ-mixin))))
              (instance (make-instance ',name :organ organ :invoke-when ',behavior)))

         (log-for (trace) "Updating class of ~A" organ)
         (change-class organ new-organ-class)

         (log-for (trace) "Storing self as a behavior")
         (pushnew instance (behaviors organ) :test #'eql :key #'type-of)

         (values instance organ)))


     (defmethod initialize-instance :before ((behavior ,name) &key)
       "Bind the funcallable lambda to the `behavior' instance."
       (log-for (trace) "Binding funcallable lambda to ~A" behavior)
       (c2mop:set-funcallable-instance-function
        behavior
        #'(lambda ,invoke-lambda (progn (log-for (trace) "Behavior ~A running" (symbol-name ',name))
                                        ,@body))))))

(defmethod act-on-event :after ((organ behaving-organ-mixin) event)
  "Deterimine if this event should result in a behavior invocation"
  (log-for (warn) "TODO: Call invoke-p for each behavior and invoke it if need be.")
  (let (fired)
    (dolist (behavior (behaviors organ) fired)
      (log-for (trace) "Testing ~A of ~A" behavior organ)
      (when (funcall (invoke-p behavior) event)
        (log-for (trace) " Passed, invoking")
        (funcall behavior organ)
        (push behavior fired)))))

