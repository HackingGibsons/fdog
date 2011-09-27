(in-package :fdog-forwarder)

;; Forwarder endpoint creation
(defmethod init-context ((endpoint forwarder-engine-endpoint))
  (terminate-context endpoint)
  (log-for (trace) "Creating ZMQ context for endpoint with ~A threads" (endpoint-context-threads endpoint))
  (setf (endpoint-context endpoint)
        (zmq:init (endpoint-context-threads endpoint))))

(defmethod terminate-context ((endpoint forwarder-engine-endpoint))
  (when (endpoint-context endpoint)
    (terminate-sockets endpoint)
    (log-for (trace) "Terminating endpoint context: ~A" endpoint)
    (zmq:term (endpoint-context endpoint))
    (setf (endpoint-context endpoint) nil)
    (log-for (trace) "Terminated: ~A" endpoint)))

(defmethod forwarder-engine-forwarder ((endpoint forwarder-engine-endpoint))
  (forwarder-engine-forwarder (endpoint-engine endpoint)))

(defmethod maybe-linger-socket (socket)
  (and *endpoint-socket-linger*
       (zmq:setsockopt socket zmq:linger *endpoint-socket-linger*))
  socket)

(defmethod forwarder-forward-to ((endpoint forwarder-engine-endpoint))
  (forwarder-forward-to (or (endpoint-alias endpoint)
                            (forwarder-engine-forwarder endpoint))))

(defmethod forwarder-listen-on ((endpoint forwarder-engine-endpoint))
  (forwarder-listen-on (or (endpoint-alias endpoint)
                            (forwarder-engine-forwarder endpoint))))

(defmethod init-sockets ((endpoint forwarder-engine-endpoint))
  (log-for (trace) "Initializing sockets of endpoing ~A" endpoint)
  (terminate-sockets endpoint)

  (with-slots (context) endpoint

    ;; Client socks
    (with-slots (request-sock response-sock) endpoint
      (let* ((forwarder (forwarder-engine-forwarder endpoint))
             (req-addr (make-local-endpoint :port (forwarder-forward-to endpoint)))
             (res-addr (make-local-endpoint :port (forwarder-listen-on endpoint))))
        (setf request-sock
              (maybe-linger-socket (zmq:socket context zmq:push))
              response-sock
              (maybe-linger-socket (zmq:socket context zmq:sub)))

        (log-for (trace) "Binding forwarder [~A] client sockets Req: ~A and Rep: ~A"
                 (fdog-forwarder-name forwarder) req-addr res-addr)
        (zmq:setsockopt response-sock zmq:subscribe "")
        (zmq:bind request-sock req-addr)
        (zmq:bind response-sock res-addr)
        (log-for (trace) "Client socket binding complete.")))

    ;; Request proxy
    (with-slots (request-proxy-addr request-proxy-sock) endpoint
      (setf request-proxy-addr
            (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))) ;; TODO: Use a different generator than next-handler-port
;;            (format nil "ipc:///tmp/~A.fdog.sock" (uuid:make-v4-uuid)))
      (log-for (trace) "Chosen the address: ~A as the request-proxy, binding" request-proxy-addr)
      (setf request-proxy-sock
            (maybe-linger-socket (zmq:socket context zmq:pull)))

      (zmq:bind request-proxy-sock request-proxy-addr)
      (log-for (trace) "Request proxy bound."))

    ;; Response proxy
    (with-slots (response-proxy-addr response-proxy-sock) endpoint
      (setf response-proxy-addr
            (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))) ;; TODO: see above
      (log-for (trace) "Chosen the address: ~A as the response-proxy, binding" response-proxy-addr)
      (setf response-proxy-sock
            (maybe-linger-socket (zmq:socket context zmq:pub)))

      (log-for (trace) "Response proxy bound: ~A" (zmq:bind response-proxy-sock response-proxy-addr))

      (log-for (trace) "Connecting reply socket to relevant M2 endpoints.")
      (dolist (addr (mapcar #'mongrel2-handler-recv-spec
                            (forwarder-engine-handlers (endpoint-engine endpoint))))
        (log-for (trace) "  Connecting to: ~A" addr)
        (zmq:connect response-proxy-sock addr)))))

(defmethod terminate-sockets ((endpoint forwarder-engine-endpoint))
  (log-for (trace) "Terminating sockets of endpoint: ~A" endpoint)
  (with-slots (request-proxy-sock response-proxy-sock request-sock response-sock) endpoint
    (log-for (dribble) "Closing ~A sockets." (length (remove nil (list request-proxy-sock response-proxy-sock request-sock response-sock))))
    (mapcar #'zmq:close
            (remove nil (list request-proxy-sock response-proxy-sock request-sock response-sock)))
    (setf request-proxy-sock nil
          response-proxy-sock nil
          request-sock nil
          response-sock nil)))

;; Forwarder endpoint operation
(defmethod engine-endpoint-running-p ((endpoint forwarder-engine-endpoint))
  (with-slots (request-device response-device) endpoint
    (or (and response-device
             (threadp response-device)
             (thread-alive-p response-device))
        (and request-device
             (threadp request-device)
             (thread-alive-p request-device)))))

(defmethod make-request-device ((endpoint forwarder-engine-endpoint))
  #'(lambda ()
      (log-for (trace) "Starting request proxy device")
      (labels ((run-device ()
                 (handler-case
                     (zmq:device zmq:streamer
                                 (endpoint-proxy-sock endpoint)
                                 (endpoint-request-sock endpoint))
                   (simple-error (c)
                     (if (= (sb-alien:get-errno) sb-posix:eintr)
                         t
                         (prog1 nil
                           (log-for (warn) "Device exited with condition: ~A" c)
                           (signal c)))))))
        (loop while (run-device) do
             (log-for (trace) "Device restarting due to system weather.")))
      (log-for (trace) "Device has terminated.")))

(defmethod make-response-device ((endpoint forwarder-engine-endpoint))
  (let ((client-response (endpoint-response-sock endpoint))
        (response-proxy (endpoint-response-proxy-sock endpoint)))
    #'(lambda ()
        (labels ((run-device ()
                   (handler-case
                       (zmq:device zmq:forwarder
                                   client-response
                                   response-proxy)
                     (simple-error (c)
                       (if (= (sb-alien:get-errno) sb-posix:eintr)
                           t
                           (prog1 nil
                             (log-for (warn) "Response device exited with condition: ~A" c)
                             (signal c)))))))
          (loop while (run-device) do
               (log-for (trace) "Response device restarting due to system weather.")))
        (log-for (trace) "Response device has terminated."))))

(defmethod engine-endpoint-start ((endpoint forwarder-engine-endpoint))
  (log-for (trace) "Starting endpoint: ~A" endpoint)
  (unless (engine-endpoint-running-p endpoint)
    (init-context endpoint)
    (init-sockets endpoint)

    (let* ((name (fdog-forwarder-name (forwarder-engine-forwarder (endpoint-engine endpoint))))
           (name (if (endpoint-alias endpoint)
                     (format nil "~A-alias-~A" name (fdog-forwarder-alias-name (endpoint-alias endpoint)))
                     name)))
      (log-for (trace) "Starting engine endpoint with name: ~A" name)
      (setf (endpoint-request-device endpoint)
            (make-thread (make-request-device endpoint)
                         :name (format nil "engine-endpoint-device-request-~A" name))

            (endpoint-response-device endpoint)
            (make-thread (make-response-device endpoint)
                         :name (format nil "engine-endpoint-device-response-~A" name)))
      :started)))

(defmethod engine-endpoint-stop ((endpoint forwarder-engine-endpoint))
  (log-for (trace) "Stopping endpoint! => ~A" endpoint)
  (log-for (trace) "Running: ~A" (engine-endpoint-running-p endpoint))
  (when (engine-endpoint-running-p endpoint)
    (log-for (trace) "Preparing to terminate thread.")
    (log-for (warn) "This is very much the wrong way to go about this one.")
    (when (thread-alive-p (endpoint-request-device endpoint))
      (destroy-thread (endpoint-request-device endpoint)))
    (when (thread-alive-p (endpoint-request-device endpoint))
      (destroy-thread (endpoint-response-device endpoint))))
  (log-for (trace) "Destroying 0mq endpoint")
  (terminate-sockets endpoint)
  (terminate-context endpoint)
  :stopped)

(defmethod make-request-rewriter-for ((multibridge multibridge))
  "Make a request rewriter lambda mapping (handler request raw) => (handler' request' raw')"
  (let ((prefix-re (format nil "^~A" (multibridge-path multibridge))))
    (labels ((rewrite-request (raw)
               (destructuring-bind (sender connection-id path rest) (m2cl::token-parse-n raw 3)
                 (flet ((perform-rewrite ()
                          (let ((json:*json-identifier-name-to-lisp* 'identity)
                                (json:*lisp-identifier-name-to-json* 'identity))
                            (multiple-value-bind (headers-string rest)
                                (m2cl::netstring-parse rest)
                              (let ((headers (json:decode-json-from-string headers-string)))
                                (when (cdr (assoc :PATH headers))
                                  (setf (cdr (assoc :PATH headers))
                                        (ppcre:regex-replace prefix-re (cdr (assoc :PATH headers)) "/")))

                                (when (cdr (assoc :URI headers))
                                  (setf (cdr (assoc :URI headers))
                                        (ppcre:regex-replace prefix-re (cdr (assoc :URI headers)) "/")))

                                (setf headers-string (json:encode-json-to-string headers))

                                (babel:string-to-octets (format nil "~A ~A ~A ~A:~A,~A"
                                                               sender connection-id (ppcre:regex-replace prefix-re path "/")
                                                               (length headers-string) headers-string
                                                               (babel:octets-to-string rest))))))))

                   (if (ppcre:scan prefix-re path)
                       (perform-rewrite)
                       raw)))))
      #'(lambda (handler request raw)
          (list handler
                request
                (rewrite-request raw))))))

(defmethod request-forwarding-address ((endpoint forwarder-engine-endpoint))
  "A method to override to cause requests to be proxied to a different address by the handlers."
  (endpoint-proxy-addr endpoint))

(defmethod make-request-forwarder-for (multibridge)
  "Make a request forwarder lambda mapping (handler request raw) as identity
and forwarding the request according to where this endpoint wants it to go."
  (labels ((matching-endpoint-p (alias request)
             (let* ((method (cdr (assoc :method (m2cl:request-headers request))))
                    (method (and method (intern method :keyword))))
               (log-for (trace) "Considering: ~A against ~A" alias request)
               (log-for (trace) "Method: (~A)~A" (type-of method) method)
               (log-for (trace) "Path: ~A" (m2cl:request-path request))
               (when (and (or (null (fdog-forwarder-alias-method alias))
                              (string-equal (fdog-forwarder-alias-method alias) method))
                          (ppcre:scan (or (fdog-forwarder-alias-match alias) "")
                                      (m2cl:request-path request)))
                 (log-for (trace) "==> Match")
                 t)))

           (select-endpoint (req raw)
             (declare (ignorable req))0

             (let ((engine (multibridge-engine multibridge))
                   (new-req (m2cl::request-parse raw)))
               (or (loop for alias in (forwarder-engine-alias-endpoints engine)
                      if (matching-endpoint-p (car alias) new-req)
                        return (cdr alias))
                   (forwarder-engine-endpoint (multibridge-engine multibridge))))))

    (lambda (handler request raw)
      (let ((endpoint (select-endpoint request raw)))
        (with-slots (context request-proxy-addr) endpoint
          (zmq:with-socket (forward context zmq:push)
            (maybe-linger-socket forward)
            (zmq:connect forward (request-forwarding-address endpoint))
            (log-for (trace) "Forwarding request to ~A for endpoint ~A" (request-forwarding-address endpoint) endpoint)
            (log-for (trace) "Send result: ~A"
                     (zmq:send! forward (make-instance 'zmq:msg :data raw)))))
        (list handler request raw)))))

(defmethod multibridge-request-proccessors ((multibridge multibridge))
  "Return a list of functions accepting (handler request raw) and returning the same.
The result of calling the first will be the parameters to the second, and so forth."
  (list
   (make-request-rewriter-for multibridge)
   (make-request-forwarder-for multibridge)))
