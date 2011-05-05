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

   (processor :initform (lambda (handler request raw) (declare (ignorable handler request raw)))
              :initarg :processor
              :initarg :proc)

   (responder :initform nil
              :accessor request-handler-thread
              :accessor request-handler-responder)
   (responder-lock :initform (make-lock "Responder Loop Lock")
                   :accessor request-handler-lock))
  (:documentation "Class wrapping the creation of request handlers"))

;; TODO: Handler won't stop until next request, if we're going to try to be polite about it
(defmethod request-handler-stop ((handler request-handler))
  (when (request-handler-running-p handler)
    (with-lock-held ((request-handler-lock handler))
      (join-thread (request-handler-thread handler))
      :stopped)))

(defmethod request-handler-start ((req-handler request-handler))
  (unless (request-handler-running-p req-handler)
    (with-slots (ident sub-address pub-address responder responder-lock processor) req-handler
      (labels ((wait-get-process-request (handler)
                 (log-for (trace) "Waiting for request on handler: ~A" ident)
                 (multiple-value-bind (req raw) (m2cl:handler-receive handler)
                   (funcall processor handler req raw)))

               (poller () (m2cl:with-handler (handler ident sub-address pub-address)
                            (log-for (trace) "Starting to handle things")
                            (loop while (acquire-lock responder-lock nil) do
                                 (unwind-protect
                                      (wait-get-process-request handler)
                                   (release-lock responder-lock))))))
        (setf responder (make-thread #'poller :name (format nil "fdog-handler-poller(~A)" ident)))))))

(defmethod request-handler-running-p ((handler request-handler))
  (with-slots (responder) handler
    (and responder
         (threadp responder)
         (thread-alive-p responder))))

(defun run (&rest args &key &allow-other-keys)
  (labels ((response () (format nil "~A:~A" (get-universal-time) (current-thread)))
           (req-fun (handler request raw) (declare (ignorable raw))
             (m2cl:handler-send-http handler (response) :request request)
             (m2cl:handler-close handler :request request)))
    (let ((handler (make-instance 'request-handler :ident *ident* :proc #'req-fun
                                  :sub *m2-send* :pub *m2-recv*)))
      (log-for (dribble) "Starting request responder")
      (request-handler-start handler)
      handler)))


