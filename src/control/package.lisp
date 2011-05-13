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

   (processor :initform (lambda (req-handler request raw) (declare (ignorable req-handler request raw)))
              :initarg :processor
              :initarg :proc
              :accessor request-handler-processor)

   (responder :initform nil
              :accessor request-handler-thread
              :accessor request-handler-responder)
   (responder-handler :initform nil
                      :accessor request-handler-responder-handler)
   (responder-lock :initform (make-lock "Responder Loop Lock")
                   :accessor request-handler-lock))
  (:documentation "Class wrapping the creation of request handlers"))

(defmethod request-handler-wait-get-process ((req-handler request-handler))
  (flet ((s2us (s) (round (* s 1000000))))
    (let ((m2-handler (request-handler-responder-handler req-handler)))
      (multiple-value-bind (req raw) (m2cl:handler-receive m2-handler (s2us 0.01))
        (when req
          (funcall (request-handler-processor req-handler) req-handler req raw))))))

(defmethod request-handler-start ((req-handler request-handler))
  (when (request-handler-running-p req-handler) (return-from request-handler-start))
  (with-slots (ident sub-address pub-address responder responder-lock processor) req-handler
    (labels ((poller () (m2cl:with-handler (handler ident sub-address pub-address)
                          (setf (request-handler-responder-handler req-handler) handler)
                          (loop while (acquire-lock responder-lock nil) do
                               (unwind-protect
                                    (handler-case (request-handler-wait-get-process req-handler)
                                      (simple-error (c) (let ((r (find-restart :terminate-thread c)))
                                                          (format t "Restart: ~A Cond: ~A" r c)
                                                          (signal c))))
                                 (release-lock responder-lock)))
                          (setf (request-handler-responder-handler req-handler) nil))))

      (setf responder (make-thread #'poller :name (format nil "fdog-handler-poller(~A)" ident))))))

;; TODO: Handler won't stop until next request, if we're going to try to be polite about it
;;       Though we now default to a 10ms poll interval, giving us a max 10ms pause...
(defmethod request-handler-stop ((handler request-handler))
  (when (request-handler-running-p handler)
    (with-lock-held ((request-handler-lock handler))
      (join-thread (request-handler-thread handler))
      :stopped)))

(defmethod request-handler-running-p ((handler request-handler))
  (with-slots (responder) handler
    (and responder
         (threadp responder)
         (thread-alive-p responder))))


;;; Scaffold
(defun response ()
  (format nil "~A:~A" (get-universal-time) (current-thread)))

(defun req-fun (req-handler request raw)
  ;; (log-for (dribble) "Raw request: ~A" (flex:octets-to-string raw))
  ;; (log-for (dribble) "Cooked request: ~A" (or (and request
  ;;                                                  (list
  ;;                                                   :headers (m2cl:request-headers request)
  ;;                                                   :body (m2cl:request-body request)
  ;;                                                   :data (m2cl:request-data request)))
  ;;                                             "Is nil"))
  (let ((m2-handler (request-handler-responder-handler req-handler)))
    (unless (m2cl::request-disconnect? request)
      (m2cl:handler-send-http m2-handler (response) :request request)
      (m2cl:handler-close m2-handler :request request))))



(defun run (&rest args &key &allow-other-keys)
  (declare (ignorable args))
  (let ((handler (make-instance 'request-handler :ident *ident* :proc 'req-fun
                                :sub *m2-send* :pub *m2-recv*)))
    (log-for (dribble) "Starting request responder")
    (request-handler-start handler)
    handler))
