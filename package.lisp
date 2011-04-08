;;;; package.lisp

(defpackage #:fdog
  (:use #:cl)
  (:export :init)
  (:shadowing-import-from #:clsql))

