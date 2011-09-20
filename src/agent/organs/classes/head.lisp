(in-package :agent)

(defclass agent-head (standard-beating-organ)
  ((organs :initform (make-hash-table :test 'equalp)
           :accessor agent-organs))

  (:documentation "This organ is responsible for decision making and task scheduling, but none of the work.")
  (:default-initargs . (:tag :head)))

(defclass standard-behavior (c2mop:funcallable-standard-object)
  ((organ :initarg :organ
          :accessor behavior-organ)
   (invoke-when :initarg :invoke-when
                :accessor invoke-when)
   (invoke-p :accessor invoke-p)))

(defmethod behavior-compile-invoke-p (behavior description)
  (eval `(let ((behavior ,behavior))
           (macrolet ((:interval (definition verb noun)
                        `(progn
                           (lambda (event)
                             (log-for (warn) "TODO: The compiler does nothing: ~A => ~A" event behavior)
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
     (defclass ,name (standard-behavior)
       ()
       (:metaclass c2mop:funcallable-standard-class))

     (defmethod ,(intern (string-upcase (concatenate 'string "make-" (symbol-name name)))) (organ &key)
       "Make an instance of a behavior class"
       (log-for (trace) "Making ~A" (symbol-name ',name))
       (make-instance ',name :organ organ :invoke-when ',behavior))

     (defmethod initialize-instance :before ((behavior ,name) &key)
       (log-for (trace) "Binding funcallable lambda to ~A" behavior)
       (c2mop:set-funcallable-instance-function
        behavior
        #'(lambda ,invoke-lambda (progn (log-for (trace) "Behavior ~A running" (symbol-name ',name))
                                        ,@body))))))

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  :oh-hello)
