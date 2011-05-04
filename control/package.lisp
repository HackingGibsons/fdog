(defpackage :fdog-control
  (:use :cl :fdog-models)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defparameter *ident* "control-ident")
(defparameter *m2-send* "tcp://127.0.0.1:13375")
(defparameter *m2-recv* "tcp://127.0.0.1:13372")

(defun run (&rest args &key &allow-other-keys)
  (log-for (info) "Booting control handler with: ~A" args))
