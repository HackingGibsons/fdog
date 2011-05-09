(in-package :fdog-tests)

;;; Mixin macros here
(defmacro +db/connected (&body body)
  `(let ()
     ,@body))

;;; Foxtures/Mixtures
(def-mixture db/connected ()
    (+db/connected)
  (&body))
