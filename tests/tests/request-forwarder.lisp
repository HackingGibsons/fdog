(in-package :afdog-tests)

(def-test (request-forwarding-agent-starts :group request-forwarder-agent-tests)
    (:values (:eql :starts)
             (:eql :running))
  (wait-for-agent (request-forwarder-uuid) :starts)
  (and (running-p request-forwarder-runner) :running))

(def-test (request-forwarder-agent-announces-provides :group request-forwarder-agent-tests)
    (:values (:equalp "forwarder-test-default")
             :true)

  (wait-for-agent-message (request-forwarder-uuid) (msg)
    (let* ((info (getf msg :info))
           (provides (getf info :provides)))
         (when provides
           (getf provides :request-processing))))

  (wait-for-agent-message (request-forwarder-uuid) (msg)
    (let* ((info (getf msg :info))
           (provides (getf info :provides)))
      (getf provides :redis))))

(def-test (request-forwarder-agent-announces-provides-forwarding :group request-forwarder-agent-tests)
    (:seq (:eql :forwarder) (:predicate stringp)
          (:eql :route) (:predicate stringp)
          (:eql :path) (:predicate string)
          (:eql :endpoints) (:seq (:eql :default)
                                  (:seq (:eql :push) (:predicate stringp)
                                        (:eql :sub) (:predicate stringp))))
  (wait-for-agent-message (request-forwarder-uuid) (msg)
    (let* ((info (getf msg :info))
           (provides (getf info :provides)))
         (when provides
           (getf provides :forwarding)))))

(def-test (request-forwarder-agent-connects :group request-forwarder-agent-tests)
    (:eql :connected-to-one)
  (wait-for-agent-message (request-forwarder-uuid :timeout 30) (msg)
    (let* ((info (getf msg :info))
           (requesticle (getf info :requesticle)))
         (when (>= (getf requesticle :peers) 1)
          :connected-to-one))))

(def-test (request-forwarder-agent-signals-client-push :group request-forwarder-agent-tests)
    (:values (:seq (:eql :forwarder) (:predicate stringp)
                   (:eql :route) (:predicate stringp)
                   (:eql :path) (:predicate string)
                   (:eql :endpoints) (:seq (:eql :default)
                                           (:seq (:eql :push) (:predicate stringp)
                                                 (:eql :sub) (:predicate stringp))))
             (:eql :ready)
             (:eql :still-there))

  (wait-for-agent-message (request-forwarder-uuid :timeout 30) (msg)
    (let* ((info (getf msg :info))
           (provides (getf info :provides)))
      (when provides
        (getf provides :forwarding))))

  (zmq:with-context (ctx 1)
    (zmq:with-socket (pull ctx :pull)
      (let ((addr (wait-for-agent-message (request-forwarder-uuid) (msg)
                    (getf (cadr (getf (getf (getf (getf msg
                                                        :info)
                                                        :provides)
                                                        :forwarding)
                                                        :endpoints))
                          :push))))
        (if addr
          (with-agent-conversation (m e) request-forwarder-uuid
            (do* ((connected (zmq:connect pull addr) connected)
                  (msg (parse-message (read-message m))
                       (parse-message (read-message m)))
                  (transition (getf msg :transition)
                              (getf msg :transition)))
                 (transition
                  (getf transition :state))))
          :no-addr-found))))

  (wait-for-agent (request-forwarder-uuid) :still-there))

(def-test (request-forwarder-agent-forwards-request :group request-forwarder-agent-tests)
    (:values (:eql :connected)
             (:eql :requested)
             (:eql :forwarded))

  (zmq:with-context (ctx 1)
    (zmq:with-socket (pull ctx :pull)
      (values
       (let ((addr (wait-for-agent-message (request-forwarder-uuid) (msg)
                     (getf (cadr (getf (getf (getf (getf msg
                                                         :info)
                                                         :provides)
                                                         :forwarding)
                                                         :endpoints))
                           :push))))
         (if addr
             (and (zmq:connect pull addr)
                  :connected)
             :no-addr-found))

       (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" forwarder-agent:*forwarder-server-port*))
         (write-string (http-request-string "/api/" :host "api.example.com") (usocket:socket-stream sock))
         (force-output (usocket:socket-stream sock))
         :requested)

       (zmq:with-poll-items (items nb-items)
           ((pull :pollin))
         (let ((nb-signaled-items (zmq:poll items nb-items (round (* 1 1000000)))))
           (if (> nb-signaled-items 0)
               (and (not (zerop (length (zmq:recv! pull :array))))
                    :forwarded)
               :timeout)))))))

(def-test (request-forwarder-agent-adds-request-id-header :group request-forwarder-agent-tests)
    (:values (:eql :connected)
             (:eql :requested)
             (:eql :forwarded-with-header))

  (zmq:with-context (ctx 1)
    (zmq:with-sockets ((pull ctx :pull) (pub ctx :pub))
      (let* ((addrs (wait-for-agent-message (request-forwarder-uuid) (msg)
                      (cadr (getf (getf (getf (getf msg :info) :provides) :forwarding) :endpoints))))
             (push-addr (getf addrs :push))
             (sub-addr (getf addrs :sub))
             (handler (and push-addr sub-addr (make-instance 'm2cl:handler :pull pull :pub pub))))

        (values
         (if handler
             (and (zmq:connect pull push-addr)
                  (zmq:connect pub sub-addr)
                  :connected)
             :no-addr-found)

         (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" forwarder-agent:*forwarder-server-port*))
           (write-string (http-request-string "/api/" :host "api.example.com") (usocket:socket-stream sock))
           (force-output (usocket:socket-stream sock))
           :requested)

         (or (when-bind request (m2cl:handler-receive handler :timeout 3000000)
               (if (assoc "x-fdog-request-id" (m2cl:request-headers request) :test #'string-equal)
                   :forwarded-with-header
                   :forwarded-without-x-fdog-request-id-header))
               :didnt-get-request))))))

(def-test (request-forwarder-agent-handles-delivery-failure :group request-forwarder-agent-tests)
    (:values (:eql :requested)
             (:eql :still-there))

  (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" forwarder-agent:*forwarder-server-port*))
    (write-string (http-request-string "/api/" :host "api.example.com") (usocket:socket-stream sock))
    (force-output (usocket:socket-stream sock))
    :requested)

  (wait-for-agent (request-forwarder-uuid :timeout 10) :still-there))

(def-test (request-forwarder-agent-request->response-forwarding :group request-forwarder-agent-tests)
    (:values (:eql :connected)
             (:eql :waited)
             (:eql :requested)
             (:eql :handled)
             (:eql :replied))
  (zmq:with-context (ctx 1)
    (zmq:with-sockets ((pull ctx :pull) (pub ctx :pub))
      (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" forwarder-agent:*forwarder-server-port*
                                                                   :element-type 'flex:octet))
        (let* ((addrs (wait-for-agent-message (request-forwarder-uuid) (msg)
                        (cadr (getf (getf (getf (getf msg :info) :provides) :forwarding) :endpoints))))
               (push-addr (getf addrs :push))
               (sub-addr (getf addrs :sub))
               (handler (and push-addr sub-addr (make-instance 'm2cl:handler :pull pull :pub pub))))

          (values
           (if handler
               (and (zmq:connect pull push-addr)
                    (zmq:connect pub sub-addr)
                    :connected)
               :no-addr-found)

           (wait-for-agent (request-forwarder-uuid) :waited)

           (prog1 :requested
             (write-sequence (flex:string-to-octets (http-request-string "/api/test-reply/" :host "api.example.com"))
                             (usocket:socket-stream sock))
             (force-output (usocket:socket-stream sock)))

           (or (when-bind request (m2cl:handler-receive handler :timeout 3000000)
                 (m2cl:handler-send-http handler "OHI" :request request)
                 :handled)
               :didnt-get-request)

           (handler-case
               (bt:with-timeout (3)
                 (let* ((status (drakma::read-status-line (usocket:socket-stream sock)))
                        (headers (and (member 200 status :test #'equalp)
                                      (drakma::read-http-headers (usocket:socket-stream sock))))
                        (body (and headers
                                   (drakma::read-body (flex:make-flexi-stream (chunga:make-chunked-stream (usocket:socket-stream sock)))
                                                      headers nil t))))
                   (if (equalp body "OHI")
                       :replied
                       `(:bad-reply ,status ,headers ,body))))
             (bt:timeout () :TIMEOUT-reading-HTTP-response))))))))

(def-test (request-forwarder-agent-strips-prefix :group request-forwarder-agent-tests)
    (:values (:eql :connected)
             (:eql :requested)
             (:eql :forwarded-with-/))

  (zmq:with-context (ctx 1)
    (zmq:with-sockets ((pull ctx :pull) (pub ctx :pub))
      (let* ((addrs (wait-for-agent-message (request-forwarder-uuid) (msg)
                      (cadr (getf (getf (getf (getf msg :info) :provides) :forwarding) :endpoints))))
             (push-addr (getf addrs :push))
             (sub-addr (getf addrs :sub))
             (handler (and push-addr sub-addr (make-instance 'm2cl:handler :pull pull :pub pub))))

        (values
         (if handler
             (and (zmq:connect pull push-addr)
                  (zmq:connect pub sub-addr)
                  :connected)
             :no-addr-found)

         (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" forwarder-agent:*forwarder-server-port*))
           (write-string (http-request-string "/api/" :host "api.example.com") (usocket:socket-stream sock))
           (force-output (usocket:socket-stream sock))
           :requested)

         (or (when-bind request (m2cl:handler-receive handler :timeout 3000000)
               (if (string= (m2cl:request-path request) "/")
                   :forwarded-with-/
                   :forwarded-without-stripping))
               :didnt-get-request))))))
