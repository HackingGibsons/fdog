(in-package :fdog-tests)

(defmacro def-mixture (name arglist mixins &body body)
  "Make a fixture mixing in the fixture macros."
  (let* ((g!eval-body (gensym "eval-body"))
         (callchain `(,g!eval-body)))
      (dolist (mixin (reverse mixins) callchain)
        (setf callchain `(,mixin ,callchain)))

      `(def-fixture ,name ,arglist
         (macrolet ((,g!eval-body () '(progn ,@body)))
           ,callchain))))
