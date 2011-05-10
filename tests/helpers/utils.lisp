(in-package :fdog-tests)

#+sbcl
(defmacro def-fixture (name args &body body)
  "Defines a fixture named NAME. A fixture is very much like a
macro but is used only for simple templating. A fixture created
with DEF-FIXTURE is a macro which can use the special macrolet
 to specify where the body should go.

See Also: WITH-FIXTURE


*note* that this overrides the default 5am DEF-FIXTURE. 5am's
DEF-FIXTURE does not remove the old instance of the fixture.

Modified def-fixture from: http://www.cliki.net/FiveAM
Should stop SBCL from complainig about fixture edits without
restarting the entire VM, which becomes tiresome if you're
developing fixtures.
by: Nixeagle"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rem-fixture ',name)
     (setf (get-fixture ',name) (cons ',args ',body))
     ',name))

(defmacro def-mixture (name arglist mixins &body body)
  "Make a fixture mixing in the fixture macros."
  (let* ((g!eval-body (gensym "eval-body"))
         (callchain `(,g!eval-body)))
      (dolist (mixin (reverse mixins) callchain)
        (setf callchain `(,mixin ,callchain)))

      `(def-fixture ,name ,arglist
         (macrolet ((,g!eval-body () '(progn ,@body)))
           ,callchain))))
