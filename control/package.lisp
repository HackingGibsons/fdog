(defpackage :fdog-control
  (:use :cl :fdog-models)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defparameter *ident* "control-ident")
(defparameter *m2-send* "tcp://127.0.0.1:13375")
(defparameter *m2-recv* "tcp://127.0.0.1:13372")

(defun run (&rest args &key &allow-other-keys)
  (log-for (trace) "Booting control handler with: ~A" args)
  (m2cl:with-handler (handler *ident* *m2-send* *m2-recv*)
    (loop
       (log-for (trace) "Waiting for request!")
       (let ((request (m2cl:handler-receive handler)))
         (log-for (trace) "Got request!")
         (m2cl:handler-send-http handler "I WIN!" :request request)
         (m2cl:handler-close handler :request request)
         (log-for (trace) "Response sent.")))))


