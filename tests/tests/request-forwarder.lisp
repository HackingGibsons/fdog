(in-package :afdog-tests)

(def-test (request-forwarding-agent-starts :group request-forwarder-agent-tests)
    (:values (:eql :starts)
             (:eql :running))
  (with-agent-conversation (m e) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info) (getf msg :info)))
         ((and info)
          :starts)))

   (and (running-p request-forwarder-runner) :running))

(def-test (request-forwarder-agent-announces-provides :group request-forwarder-agent-tests)
    (:equalp "forwarder-test-default")
  (with-agent-conversation (m e) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :request-processing)))))

(def-test (request-forwarder-agent-announces-provides-forwarding :group request-forwarder-agent-tests)
    (:seq (:eql :forwarder) (:predicate stringp)
          (:eql :route) (:predicate stringp)
          (:eql :path) (:predicate string)
          (:eql :endpoints) (:seq (:eql :default)
                                  (:seq (:eql :push) (:predicate stringp)
                                        (:eql :sub) (:predicate stringp))))
  (with-agent-conversation (m e) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :forwarding)))))

(def-test (request-forwarder-agent-connects :group request-forwarder-agent-tests)
    (:eql :connected-to-one)
  (with-agent-conversation (m e :timeout 30) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (requesticle (getf info :requesticle)
                       (getf info :requesticle)))
         ((>= (getf requesticle :peers) 1)
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

  (with-agent-conversation (m e) request-forwarder-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :forwarding))))

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
         (write-string (http-request-string "/" :host "api.example.com") (usocket:socket-stream sock))
         (force-output (usocket:socket-stream sock))
         :requested)

       (zmq:with-poll-items (items nb-items)
           ((pull :pollin))
         (let ((nb-signaled-items (zmq:poll items nb-items (round (* 1 1000000)))))
           (if (> nb-signaled-items 0)
               (and (not (zerop (length (zmq:recv! pull :array))))
                    :forwarded)
               :timeout)))))))

(def-test (request-forwarder-agent-handles-delivery-failure :group request-forwarder-agent-tests)
    (:values (:eql :requested)
             (:eql :still-there))

  (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" forwarder-agent:*forwarder-server-port*))
    (write-string (http-request-string "/" :host "api.example.com") (usocket:socket-stream sock))
    (force-output (usocket:socket-stream sock))
    :requested)

  (wait-for-agent (request-forwarder-uuid :timeout 10) :still-there))

