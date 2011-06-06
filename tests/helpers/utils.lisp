(in-package :fdog-tests)

;; (defmacro def-fixture (name args &body body)
;;   "Defines a fixture named NAME. A fixture is very much like a
;; macro but is used only for simple templating. A fixture created
;; with DEF-FIXTURE is a macro which can use the special macrolet
;;  to specify where the body should go.

;; See Also: WITH-FIXTURE


;; *note* that this overrides the default 5am DEF-FIXTURE. 5am's
;; DEF-FIXTURE does not remove the old instance of the fixture.

;; Modified def-fixture from: http://www.cliki.net/FiveAM
;; Should stop SBCL from complainig about fixture edits without
;; restarting the entire VM, which becomes tiresome if you're
;; developing fixtures.
;; by: Nixeagle"
;;   `(eval-when (:compile-toplevel :load-toplevel :execute)
;;      (rem-fixture ',name)
;;      (setf (get-fixture ',name) (cons ',args ',body))
;;      ',name))

;; (defmacro def-mixture (name arglist mixins &body body)
;;   "Make a fixture mixing in the fixture macros."
;;   (let* ((g!eval-body (gensym "eval-body"))
;;          (callchain `(,g!eval-body)))
;;       (dolist (mixin (reverse mixins) callchain)
;;         (setf callchain `(,mixin ,callchain)))

;;       `(def-fixture ,name ,arglist
;;          (macrolet ((,g!eval-body () '(progn ,@body)))
;;            ,callchain))))

(defun http->string (url &rest args)
  "A slightly repackaged veresion of drakma's client.
Mostly exists to close the stream when required and repackage the 7-value
return as something more edible (plist). Though discarding some values that
are less usefull to string construction."
  (multiple-value-bind
        (response status-code headers uri stream must-close reason-phrase)
        (apply 'drakma:http-request `(,url ,@args))

    (when must-close (close stream))

    (values response
            (list :status-code status-code
                  :headers     headers
                  :uri         uri
                  :reason      reason-phrase))))
