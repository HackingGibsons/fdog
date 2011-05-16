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
      (declare (ignorable proc-type))
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
                  ;; TODO: This thing down here, while helping, hides some errors.
                  ;;       I need to make sure I only handler interrupted syscalls
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

(defmethod request-handler-stop ((handler request-handler))
  (when (request-handler-running-p handler)
    (handler-case (with-timeout (1)
                    (with-lock-held ((request-handler-lock handler))
                      (join-thread (request-handler-thread handler))
                      :stopped))
      (timeout ()
        (destroy-thread (request-handler-thread handler))
        :killed))))

(defmethod request-handler-running-p ((handler request-handler))
  (with-slots (responder) handler
    (and responder
         (threadp responder)
         (thread-alive-p responder))))

(defmethod request-handler-add-responder ((req-handler request-handler) responder type &key (position :beginning))
  "Add a well-formed request processor `responder' to `position' in the responder chain.
Intended to be the driver for wrapper methods that construct well-formed responders
from simpler lambdas"
  (with-slots (processors) req-handler
    (flet ((start (thing) (push thing processors))
           (end (thing) (append processors `(,thing))))
      (ecase position
        (:beginning (start (cons responder type)))
        (:end (end (cons responder type)))))))

(defmethod make-request-handler-string-responder ((req-handler request-handler) handler-fun)
  "Make a responder that calls `handler-fun' with the request and responds fully with the
return value"
  (unless (or (functionp handler-fun) (fboundp handler-fun))
    (error "Handler must be funcallable"))
  (with-slots (responder-handler) req-handler
    (lambda (handler request raw)
      (declare (ignore handler raw))
      (m2cl:handler-send-http
       responder-handler (funcall handler-fun request) :request request))))

(defmethod request-handler-add-string-responder ((req-handler request-handler) handler-fun
                                                 &key (position :beginning))
  "Add a string-responding method to the processing list"
  (request-handler-add-responder req-handler
                                 (make-request-handler-string-responder req-handler handler-fun)
                                 :string
                                 :position position))

(defmethod request-handler-make-chunked-responder/start ((req-handler request-handler) &optional chunk-start-fun)

  "Make a responder that will send chunked-encoding headers. The function `chunk-start-fun' must
return an Alist of headers/status params in the form ((:code . 200) (:status . \"OK\") ... (\"X-Some-Header\" . \"Sucks\"))
Any parameters not specified will be defaulted with no extra headers and a 200/OK response"
  (with-slots (responder-handler) req-handler
    (flet ((aval-of (key alist) (cdr (assoc key alist)))
           (a2plist (alist) (reduce (lambda (a i) (append a `(,(car i) ,(cdr i))))
                                        alist :initial-value nil)))
      (lambda (handler request raw)
        (declare (ignorable handler raw))
        (let* ((params (append (and chunk-start-fun (funcall chunk-start-fun request))
                               '((:code . 200) (:status . "OK"))))
               (codes `((:code . ,(aval-of :code params))
                        (:status . ,(aval-of :status params))))
               (headers (remove-if (lambda (param) (member (car param) (mapcar #'car codes)))
                                   params))
               (codes (a2plist codes)))

          (apply 'm2cl:handler-send-http-chunked
                 `(,responder-handler :request ,request ,@codes :headers ,headers)))))))

(defmethod request-handler-add-chunked/start ((req-handler request-handler) chunk-start-fun &key (position :beginning))

  "Add a responder that will send chunked-encoding headers. The function `chunk-start-fun' must
return an Alist of headers/status params in the form ((:code . 200) (:status . \"OK\") ... (\"X-Some-Header\" . \"Sucks\"))
Any parameters not specified will be defaulted with no extra headers and a 200/OK response"
  (request-handler-add-responder req-handler
                                 (request-handler-make-chunked-responder/start req-handler chunk-start-fun)
                                 :chunked/start
                                 :position position))

(defmethod request-handler-make-chunked-responder/chunk ((req-handler request-handler) chunk-func)
  "Make a responder lambda `chunk-func' called with the parsed `m2cl:request', the result of which will be chunk
encoded and sent to the client.  If `chunk-func' returns multiple values, the second value will be considered
in a boolean context to imply that the function should be called again, recursively."
  (with-slots (responder-handler) req-handler
    (labels ((recur (handler request raw)
               (multiple-value-bind (data again-p) (funcall chunk-func request)
                 (m2cl:handler-send-http-chunk responder-handler data :request request)
                 (when again-p (recur handler request raw)))))
      #'recur)))

(defmethod request-handler-add-chunked/chunk ((req-handler request-handler) chunk-func &key (position :beginning))
  "Add a responder lambda `chunk-func' called with the parsed `m2cl:request', the result of which will be chunk
encoded and sent to the client.  If `chunk-func' returns multiple values, the second value will be considered
in a boolean context to imply that the function should be called again, recursively."
  (request-handler-add-responder req-handler
                                 (request-handler-make-chunked-responder/chunk req-handler chunk-func)
                                 :chunked/chunk
                                 :position position))

(defmethod request-handler-make-chunked-responder/stop ((req-handler request-handler))
  "Make a stop of chunked responses responder"
  (with-slots (responder-handler) req-handler
    (lambda (handler request raw)
      (declare (ignore handler raw))
      (m2cl:handler-send-http-chunked-finish responder-handler :request request))))


(defmethod request-handler-add-chunked/stop ((req-handler request-handler) &key (position :beginning))
  "Add a stop of chunked responses responder to the chain"
  (request-handler-add-responder req-handler
                                 (request-handler-make-chunked-responder/stop req-handler)
                                 :chunked/stop
                                 :position position))

(defmethod request-handler-make-chunked-responder/trailer ((req-handler request-handler) trailer-func)
  "Make a processor lambda `trailer-fun' which when called returns an alist of trailer fields in the form
 ((key . value)..(keyn . valuen)) which will be encoded and sent to the client"
  (with-slots (responder-handler) req-handler
    (lambda (handler request raw)
      (declare (ignore handler raw))
      (let ((trailers (funcall trailer-func request)))
        (m2cl:handler-send-http-trailers responder-handler trailers :request request)))))


(defmethod request-handler-add-chunked/trailer ((req-handler request-handler) trailer-func &key (position :beginning))
  "Add a processor lambda `trailer-fun' which when called returns an alist of trailer fields in the form
 ((key . value)..(keyn . valuen)) which will be encoded and sent to the client"
  (request-handler-add-responder req-handler
                                 (request-handler-make-chunked-responder/trailer req-handler trailer-func)
                                 :chunked/trailer
                                 :position position))
