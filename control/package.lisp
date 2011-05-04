(defpackage :fdog-control
  (:use :cl :fdog-models)
  (:shadowing-import-from :log5 :log-for)
  (:export :init))
(in-package :fdog-control)

(defun init (&rest args &key &allow-other-keys)
  (log-for (dribble) "Booting control handler with: ~A" args))
