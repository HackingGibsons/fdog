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

(defmethod request-handler-add-responder ((req-handler request-handler) responder &key (position :beginning))
  "Add a well-formed request processor `responder' to `position' in the responder chain.
Intended to be the driver for wrapper methods that construct well-formed responders
from simpler lambdas"
  (with-slots (processors) req-handler
    (flet ((start (thing) (push thing processors))
           (end (thing) (append processors `(,thing))))
      (ecase position
        (:beginning (start (cons responder :string)))
        (:end (end (cons responder :string)))))))

(defmethod request-handler-add-string-responder ((req-handler request-handler) handler-fun
                                                 &key (position :beginning))
  "Add a method to the top of the responce processor list"
  (unless (or (functionp handler-fun) (fboundp handler-fun))
    (error "Handler must be funcallable"))
  (with-slots (responder-handler) req-handler
    (flet ((string-responder (handler request raw)
             (m2cl:handler-send-http
              responder-handler (funcall handler-fun request) :request request)))
      (request-handler-add-responder req-handler #'string-responder :position position))))

(defmethod request-handler-add-chunked/start ((req-handler request-handler) chunk-start-fun &key (position :beginning))

  "Add a responder that will send chunked-encoding headers. The function `chunk-start-fun' must
return an Alist of headers/status params in the form ((:code . 200) (:status . \"OK\") ... (\"X-Some-Header\" . \"Sucks\"))
Any parameters not specified will be defaulted with no extra headers and a 200/OK response"
  (with-slots (responder-handler) req-handler
    (labels ((aval-of (key alist) (cdr (assoc key alist)))
             (a2plist (alist) (reduce (lambda (a i) (append a `(,(car i) ,(cdr i))))
                                        alist :initial-value nil))

             (chunked-start-responder (handler request raw)
               (let* ((params (append (funcall chunk-start-fun request)
                                      '((:code . 200) (:status . "OK"))))
                      (codes `((:code . ,(aval-of :code params))
                               (:status . ,(aval-of :status params))))
                      (headers (remove-if (lambda (param) (member (car param) (mapcar #'car codes)))
                                          params))
                      (codes (a2plist codes)))

                 (apply 'm2cl:handler-send-http-chunked
                        `(,responder-handler :request ,request ,@codes :headers ,headers)))))

      (request-handler-add-responder req-handler #'chunked-start-responder :position position))))



(defmethod request-handler-add-chunked/chunk ((req-handler request-handler) chunk-func &key (position :beginning))
  "Add a responder lambda `chunk-func' the result of which will be chunk encoded and sent
to the client."
  :undef)

(defmethod request-handler-add-chunked/stop ((req-handler request-handler) &key (position :beginning))
  "Add a stop of chunked responses responder to the chain"
  :undef)

(defmethod request-handler-add-chunked/trailer ((req-handler request-handler) trailer-func &key (position :beginning))
  "Add a processor lambda `trailer-fun' which when called returns an alist of trailer fields in the form
 ((key . value)..(keyn . valuen)) which will be encoded and sent to the client"
  :undef)
