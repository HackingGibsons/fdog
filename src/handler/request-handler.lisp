(in-package :fdog-handler)

(defclass request-handler ()
  ((ident :initarg :ident
          :reader request-handler-ident)
   (sub-address :initarg :sub-address
                :initarg :sub
                :accessor request-handler-sub)
   (pub-address :initarg :pub-address
                :initarg :pub
                :accessor request-handler-pub)

   (processors :initform '(:close)
               :initarg :processors
               :initarg :procs
               :accessor request-handler-processors)

   (interval :initform 0.01
             :initarg :interval
             :accessor request-handler-interval
             :accessor request-handler-timeout)

   (responder :initform nil
              :accessor request-handler-thread
              :accessor request-handler-responder)
   (responder-handler :initform nil
                      :accessor request-handler-responder-handler
                      :reader request-handler-m2-handler)
   (responder-lock :initform (make-lock "Responder Loop Lock")
                   :accessor request-handler-lock))
  (:documentation "Class wrapping the creation of request handlers"))

(defmethod request-handler-process-request-with ((req-handler request-handler) processor request raw)
  "Process `request' in the context of `request-handler' with the given `processor'. Raw request
data available at `raw'"
  (let ((m2-handler (request-handler-responder-handler req-handler)))
    (destructuring-bind (proc . proc-type) processor
      (cond ((eql proc :close)
             (m2cl:handler-close m2-handler :request request)
             :closed)
            (t
             (funcall proc req-handler request raw))))))

(defmethod request-handler-wait->get->process ((req-handler request-handler))
  (flet ((s2us (s) (round (* s 1000000)))
         (form-proc-proper (procish)
           (if (consp procish) procish `(,procish . :special))))
    (let ((m2-handler (request-handler-responder-handler req-handler))
          (timeout (request-handler-timeout req-handler))
          (processors (request-handler-processors req-handler))
          proc-results)
      (multiple-value-bind (req raw) (m2cl:handler-receive m2-handler (s2us timeout))
        (when (and req (not (m2cl:request-disconnect-p req)))
          (dolist (processor processors proc-results)
            (setf proc-results
                  (append proc-results
                          `(,(request-handler-process-request-with req-handler
                                                                   (form-proc-proper processor)
                                                                   req raw))))))))))


(defmethod make-request-handler-poller ((req-handler request-handler))
  "Generate a closure to be used to create the polling thread
for the given request handler."
  (lambda ()
    (m2cl:with-handler (handler (request-handler-ident req-handler)
                                (request-handler-sub req-handler)
                                (request-handler-pub req-handler))
      (setf (request-handler-responder-handler req-handler) handler)
      (loop while (acquire-lock (request-handler-lock req-handler) nil) do
           (unwind-protect
                (handler-case (request-handler-wait->get->process req-handler)
                  (simple-error (c) (let ((r (find-restart :terminate-thread c)))
                                      (format t "Restart: ~A Cond: ~A" r c)
                                      (signal c))))
             (release-lock (request-handler-lock req-handler))))
      (setf (request-handler-responder-handler req-handler) nil))))

(defmethod request-handler-start ((req-handler request-handler))
  (when (request-handler-running-p req-handler) (return-from request-handler-start))
  (setf (request-handler-responder req-handler)
        (make-thread (make-request-handler-poller req-handler)
                     :name (format nil "fdog-handler-poller(~A)" (request-handler-ident req-handler)))))

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

(defmethod request-handler-add-string-responder ((req-handler request-handler) handler-fun
                                                 &key (position :beginning))
  "Add a method to the top of the responce processor list"
  (unless (or (functionp handler-fun) (fboundp handler-fun))
    (error "Handler must be funcallable"))
  (with-slots (processors responder-handler) req-handler
    (flet ((start (thing) (push thing processors))
           (end (thing) (append processors `(,thing)))
           (string-responder (handler request raw)
             (m2cl:handler-send-http
              responder-handler (funcall handler-fun request) :request request)))
      (ecase position
        (:beginning (start (cons #'string-responder :string)))
        (:end (end (cons #'string-responder :string)))))))
