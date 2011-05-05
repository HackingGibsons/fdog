(defpackage :fdog-control
  (:use :cl :fdog-models :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defparameter *ident* "control-ident")
(defparameter *m2-send* "tcp://127.0.0.1:13375")
(defparameter *m2-recv* "tcp://127.0.0.1:13372")

(defclass request-handler ()
  ((ident :initarg :ident)
   (sub-address :initarg :sub-address
                :initarg :sub)
   (pub-address :initarg :pub-address
                :initarg :pub)

   (processor :initform (lambda (request) (declare (ignorable request)) "")
              :initarg :processor
              :initarg :proc)

   (responder :initform nil
              :accessor request-handler-thread
              :accessor request-handler-responder)
   (responder-lock :initform nil
                   :accessor request-handler-lock))
  (:documentation "Class wrapping the creation of request handlers"))

(defmethod request-handler-stop ((handler request-handler))
  (when (request-handler-running-p handler)
    :undef))

(defmethod request-handler-start ((handler request-handler))
  (unless (request-handler-running-p handler)
    :undef))

(defmethod request-handler-running-p ((handler request-handler))
  (with-slots (responder) handler
    (and responder
         (threadp responder)
         (thread-alive-p responder))))

(defun run (&rest args &key &allow-other-keys)
  (log-for (trace) "Booting control handler with: ~A" args)
  (m2cl:with-handler (handler *ident* *m2-send* *m2-recv*)
    (loop
       (log-for (trace) "Waiting for request!")
       (multiple-value-bind (request raw) (m2cl:handler-receive handler)
         (log-for (trace) "Raw: ~A" (flexi-streams:octets-to-string raw))
         (log-for (trace) "Got request!")
         (m2cl:handler-send-http handler "I WIN!" :request request)
         (m2cl:handler-close handler :request request)
         (log-for (trace) "Response sent.")))))

