(in-package :agent)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro defbehavior (name behavior invoke-lambda &body body)
    "Generates a behavior class and and makes it funcallable with `invoke-lambda' as the lambda
list, evaluating `body' on invokation as governed by the `behavior' list. The scope will
contain `behavior' bound to the current instance of the behavior class `name'"
    `(progn
       (defclass ,name (standard-behavior
                        ,@(getf behavior :include)
                        ,@(cond ((eql (car behavior) :interval) '(interval-behavior))))
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
                                          ,@body)))))))
