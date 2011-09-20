(in-package :agent)

(defclass agent-head (standard-beating-organ)
  ((organs :initform (make-hash-table :test 'equalp)
           :accessor agent-organs))

  (:documentation "This organ is responsible for decision making and task scheduling, but none of the work.")
  (:default-initargs . (:tag :head)))

(defclass standard-behavior (c2mop:funcallable-standard-object)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmacro defbehavior (name behavior invoke-lambda &body body)
  "Generates a behavior function with a description of how it should be invoked."
  `(progn
     (defclass ,name (standard-behavior)
       ()
       (:metaclass c2mop:funcallable-standard-class))

     (defmethod initialize-instance :after ((behavior ,name) &key)
       (c2mop:set-funcallable-instance-function
        behavior
        #'(lambda ,invoke-lambda (progn (log-for (trace) "Behavior ~A running" (symbol-name ',name))
                                        ,@body))))))

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  :oh-hello)

