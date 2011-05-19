(defpackage :fdog-utils
  (:use :cl)
  (:export :flatten))
(in-package :fdog-utils)

(defun flatten (l)
  "Removes nestings from a list."
  (cond ((atom l) l)
        ((listp (car l))
         (append (flatten (car l)) (flatten (cdr l))))
        (t (append (list (car l)) (flatten (cdr l))))))
