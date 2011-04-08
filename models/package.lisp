(defpackage :fdog-models
  (:use :cl)
  (:export :connect))

(in-package :fdog-models)

(defun connect ()
  (format t "Connecting."))

(defun disconnect ()
  (format t "Disconnecting."))



