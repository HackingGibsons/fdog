(in-package :agent)

(defclass agent-head (standard-beating-organ)
  ((organs :initform (make-hash-table :test 'equalp)
           :accessor agent-organs))

  (:documentation "This organ is responsible for decision making and task scheduling, but none of the work.")
  (:default-initargs . (:tag :head)))

(defclass behaving-organ-mixin ()
  ((behaviors :initarg :behaviors :initform nil
              :accessor behaviors)))

(defclass standard-behavior (c2mop:funcallable-standard-object)
  ((organ :initarg :organ :initform nil
          :accessor behavior-organ)
   (invoke-when :initarg :invoke-when
                :accessor invoke-when)
   (invoke-p :accessor invoke-p)))

(defclass interval-behavior ()
  ((recent-events :initform nil
                  :initarg :recent-events :accessor recent-events)
   (last-invoked :initform nil
                 :initarg :last-invoked :accessor last-invoked))

  (:documentation "Mixin class for events that head their behavior with :interval"))

(defmethod behavior-compile-invoke-p (behavior description)
  (eval
   `(let ((behavior ,behavior))
      (macrolet ((:interval (definition verb noun)
                   `(let ((from (getf ',definition :from))
                          (nth (getf ',definition :nth)))
                      (lambda (event)
                        (log-for (warn) "TODO: Compiling ~A checker for every ~Ath beat from ~A" behavior nth from)
                        (when (eql (and (consp event) (car event)) from)
                          (let ((time (getf event :time)))
                            (and time
                                 (setf (recent-events behavior) (subseq (push time (recent-events behavior)) 0 nth)))
                            (log-for (warn) "TODO: Events in ~A to date: ~A" behavior (recent-events behavior))))
                        nil))))
        ,description))))

(defmethod initialize-instance :after ((behavior standard-behavior) &key)
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
       "Make an instance of a behavior class"
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
       (log-for (trace) "Binding funcallable lambda to ~A" behavior)
       (c2mop:set-funcallable-instance-function
        behavior
        #'(lambda ,invoke-lambda (progn (log-for (trace) "Behavior ~A running" (symbol-name ',name))
                                        ,@body))))))

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (warn) "TODO: Running behavior lambda for ~A" organ)
  :oh-hello)
