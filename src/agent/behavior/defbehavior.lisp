(in-package :agent)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro defbehavior (name behavior invoke-lambda &body body)
    "Generates a behavior class and and makes it funcallable with `invoke-lambda' as the lambda
list, evaluating `body' on invokation as governed by the `behavior' list. The scope will
contain `behavior' bound to the current instance of the behavior class `name'"
    (labels ((behavior-subclass (b)
               (cond ((eql (car b) :interval) '(interval-behavior))
                     ((eql (car b) :or) (loop for sub-b in (second b)
                                           appending (behavior-subclass sub-b))))))
      `(progn
         (defclass ,name (standard-behavior
                          ,@(getf behavior :include)
                          ,@(behavior-subclass behavior))
           ()
           (:metaclass c2mop:funcallable-standard-class))

         (defgeneric ,(intern (string-upcase (concatenate 'string "make-" (symbol-name name)))) (organ &key)
           (:documentation "Make an instance of a behavior class, updating `organ' to include the `behaving-organ-mixin' and attaching this behavior to the existing list.")
           (:method ((organ standard-organ) &key)
             (log-for (trace) "Making ~A" (symbol-name ',name))

             (let* ((new-organ-class (make-instance 'standard-class :name (type-of organ)
                                                    :direct-superclasses `(,(class-of organ) ,(find-class 'behaving-organ-mixin))))
                    (instance (make-instance ',name :organ organ :invoke-when ',behavior)))

               (log-for (trace) "Updating class of ~A" organ)
               (change-class organ new-organ-class)

               (log-for (trace) "Storing self as a behavior")
               (pushnew instance (behaviors organ) :test #'eql :key #'type-of)

               (values instance organ))))


         (defmethod initialize-instance :before ((behavior ,name) &key)
           "Bind the funcallable lambda to the `behavior' instance."
           (log-for (trace) "Binding funcallable lambda to ~A" behavior)
           (c2mop:set-funcallable-instance-function
            behavior
            #'(lambda ,invoke-lambda (progn ,@body))))))))
