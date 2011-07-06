(in-package :fdog-forwarder)

;; Request queuing machinery
(defun reconnect-redis-handler (c)
  (let ((reconnect (find-restart :reconnect)))
    (if reconnect
        (invoke-restart reconnect)
        (error c))))

(defmethod request-event-info ((endpoint forwarder-queue-endpoint) field &key (type :int))
  (let ((val (redis:red-hget (endpoint-queue-counter endpoint) field)))
    (ecase type
      (:int (parse-integer val))
      (:raw val))))

(defmethod request-queue-event ((endpoint forwarder-queue-endpoint) (event symbol))
  (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
    (ecase event
      (:reset (log-for (trace) "Resetting inflight request counter.")
              (redis:red-multi)
              (redis:red-hmset (endpoint-queue-counter endpoint)
                               :count 0
                               :last-pop 0
                               :last-sent 0)
              (redis:red-publish (endpoint-queue-key endpoint) :reset)
              (redis:red-exec))
      (:popped (log-for (trace) "Request popped from queue")
               (redis:red-multi)
               (redis:red-hincrby (endpoint-queue-counter endpoint) :count 1)
               (redis:red-hset (endpoint-queue-counter endpoint) :last-pop (get-internal-real-time))
               (redis:red-publish (endpoint-queue-key endpoint) :popped)
               (redis:red-exec))
      (:sent (log-for (trace) "Request sent to handler.")
             (redis:red-multi)
             (redis:red-hincrby (endpoint-queue-counter endpoint) :count -1)
             (redis:red-hset (endpoint-queue-counter endpoint) :last-sent (get-internal-real-time))
             (redis:red-publish (endpoint-queue-key endpoint) :sent)
             (redis:red-exec)))))

;; TODO: Add another generic for fdog-forwarder-name :(
(defmethod endpoint-queue-key ((endpoint forwarder-queue-endpoint))
  (with-slots (queue-prefix) endpoint
    (format nil "~A:~A:request-queue" queue-prefix
            (fdog-forwarder-name (endpoint-engine endpoint)))))

(defmethod endpoint-queue-counter ((endpoint forwarder-queue-endpoint))
  (format nil "~A:counter" (endpoint-queue-key endpoint)))

(defmethod endpoint-request-key ((endpoint forwarder-engine-endpoint) request)
  "Generate a key to store the given `request' under for the endpoint `endpoint'"
  (with-slots (request-prefix) endpoint
    (format nil "~A:~A:~A:~A" request-prefix
            (fdog-forwarder-name (endpoint-engine endpoint))
            (crypto:byte-array-to-hex-string (crypto:digest-sequence :sha256 request))
            (uuid:make-v4-uuid))))

(defmethod store-request ((endpoint forwarder-queue-endpoint) msg)
  "Store the request in redis and return a key that can be used to reffer to it."
  (let ((key (endpoint-request-key endpoint msg)))
    (log-for (trace) "Stored request for ~A" (fdog-forwarder-name (endpoint-engine endpoint)))
    (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
      (redis:red-hset key :body (flex:octets-to-string msg)))
    key))

(defmethod queue-request ((endpoint forwarder-queue-endpoint) msg)
  "Enqueue message on the endpoint to the current connected redis instance."
  (let ((request-key (store-request endpoint msg))
        (queue-key (endpoint-queue-key endpoint)))
    ;; TODO: Make sure this doesn't spin wildly if the server is outright down
    (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
      (redis:red-multi)
      (redis:red-lpush queue-key request-key)
      #|Trim|#
      (redis:red-publish (endpoint-queue-key endpoint) :pushed)
      (redis:red-exec))
    (values queue-key
            request-key)))


;; Replacement "device" to pump requests to redis and plumbing for it
(defmethod init-sockets :after ((endpoint forwarder-queue-endpoint))
  (log-for (trace) "Initing queue-specific sockets.")
  (with-slots (context request-queue-sock request-queue-addr) endpoint
    (setf request-queue-addr (make-local-endpoint :addr "127.0.0.1" :port (next-handler-port))
          request-queue-sock (zmq:socket context zmq:pull))
    (zmq:bind request-queue-sock request-queue-addr)))

(defmethod terminate-sockets :before ((endpoint forwarder-queue-endpoint))
  (log-for (trace) "Tearing down queue-specific sockets.")
  (with-slots (context request-queue-sock request-queue-addr) endpoint
    (setf request-queue-sock (and request-queue-sock
                                  (zmq:close request-queue-sock) nil)
          request-queue-addr nil)))

(defmethod make-request-queuer-device ((endpoint forwarder-queue-endpoint))
  "Make a request device that pumps requests into redis."
  #'(lambda ()
      (log-for (trace) "Starting request queue device.")
      (redis:with-recursive-connection (:host (queue-endpoint-redis-host endpoint)
                                        :port (queue-endpoint-redis-port endpoint))
        (let ((msg (make-instance 'zmq:msg)))
          (labels ((run-once ()
                     (zmq:recv (endpoint-queue-sock endpoint) msg)
                     (queue-request endpoint (zmq:msg-data-as-array msg)))

                   (handle-condition (c)
                     (or (= (sb-alien:get-errno) sb-posix:eintr)
                         (prog1 nil
                           (log-for (warn) "Queue device exited with condition: ~A" c)
                           (signal c))))

                   (run-device ()
                     (handler-case (run-once)
                       (simple-error (c) (handle-condition c)))))

            (loop while (run-device) do ':nothing))))))

;; Methods to start the request writing "device" when the endpoint is started and stopped
(defmethod make-request-writer-device ((endpoint forwarder-queue-endpoint))
  #'(lambda ()
      (log-for (trace) "Starting request queue writer device.")
      (redis:with-connection (:host (queue-endpoint-redis-host endpoint)
                              :port (queue-endpoint-redis-port endpoint))
        (let ((prev-count (request-event-info endpoint :count))
              cur-count)
          (zmq:with-socket (forward-sock (endpoint-context endpoint) zmq:push)
            (maybe-linger-socket forward-sock)
            (zmq:connect forward-sock (endpoint-proxy-addr endpoint))
            (labels ((run-once ()
                       (setf cur-count (request-event-info endpoint :count))
                       (when (> cur-count prev-count)
                         (log-for (warn) "Requests are being backlogged against a blocked writer! Now: ~A" cur-count)
                         (redis:red-subscribe (endpoint-queue-key endpoint))
                         (do ((msg (redis:expect :multi) (redis:expect :multi)))
                             ((string-equal (third msg) :sent))
                           (log-for (warn) "Waiting for next send message to resume queue pickups.."))
                         (log-for (warn) "Writing to the client has resumed, resuming queue reads.")
                         (redis:red-unsubscribe))
                       (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
                         (let* ((req-key (car (last (redis:red-brpop (endpoint-queue-key endpoint) 0))))
                                (request (and req-key (redis:red-hget req-key :body))))
                           (log-for (trace) "Got request: ~A" req-key)
                           (log-for (trace) "Request: ~A" request)

                           (and
                            (setf prev-count cur-count)
                            (request-queue-event endpoint :popped)
                            (= 0 (zmq:send forward-sock (make-instance 'zmq:msg :data request)))))))


                     (handle-condition (c)
                       (if (= (sb-alien:get-errno) sb-posix:eintr)
                           t
                           (prog1 nil
                             (log-for (warn) "Queue request writer device exited with condition: ~A" c)
                             (signal c))))

                     (run-device ()
                       (handler-case (run-once)
                         (simple-error (c) (handle-condition c)))))

              (loop while (run-device) do ':nothing)))))))


(defmethod engine-endpoint-start :after ((endpoint forwarder-queue-endpoint))
  (redis:with-recursive-connection (:host (queue-endpoint-redis-host endpoint)
                                    :port (queue-endpoint-redis-port endpoint))
    (request-queue-event endpoint :reset))
  (with-slots (request-write-device request-queue-device) endpoint
    (setf request-queue-device
          (make-thread (make-request-queuer-device endpoint)
                       :name (format nil "engine-request-queue-request-queue-device-~A"
                                     (fdog-forwarder-name (endpoint-engine endpoint))))

          request-write-device
          (make-thread (make-request-writer-device endpoint)
                       :name (format nil "engine-request-queue-request-write-device-~A"
                                     (fdog-forwarder-name (endpoint-engine endpoint)))))))

(defmethod engine-endpoint-stop :before ((endpoint forwarder-queue-endpoint))
  (log-for (trace) "Stopping the queue request writer thread for: ~A" endpoint)
  (with-slots (request-write-device request-queue-device) endpoint
    (and request-write-device
         (threadp request-write-device)
         (thread-alive-p request-write-device)
         (destroy-thread request-write-device)
         (log-for (trace) "Killed forwarder queue writer for: ~A" endpoint))
    (and request-queue-device
         (threadp request-queue-device)
         (thread-alive-p request-queue-device)
         (destroy-thread request-queue-device)
         (log-for (trace) "Killed forwarder queuer for: ~A" endpoint))
    (setf request-queue-device nil
          request-write-device nil)))

(defmethod request-forwarding-address ((endpoint forwarder-queue-endpoint))
  (log-for (trace) "Serving a queue address to forward requests to.")
  (endpoint-queue-addr endpoint))

(defmethod make-request-device ((endpoint forwarder-queue-endpoint))
  #'(lambda ()
      (log-for (trace) "Starting queued proxy request device")
      (redis:with-recursive-connection (:host (queue-endpoint-redis-host endpoint)
                                        :port (queue-endpoint-redis-port endpoint))
        (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
          (let ((msg (make-instance 'zmq:msg)))
            (labels ((run-once ()
                       (let (recv send)
                         (log-for (trace) "Reading queued request")
                         (setf recv (zmq:recv (endpoint-proxy-sock endpoint) msg))
                         (log-for (trace) "Read queued request: ~A" recv)
                         (log-for (trace) "Sending queued request.")
                         (and (= 0 recv)
                              (setf send (zmq:send (endpoint-request-sock endpoint) msg)))
                         (request-queue-event endpoint :sent)
                         (log-for (trace) "Sent queued request: ~A" send)
                         (= 0 recv send)))

                     (handle (c)
                       (declare (ignore c))
                       (= (sb-alien:get-errno) sb-posix:eintr))

                     (run-device ()
                       (handler-case (run-once)
                         (simple-error (c) (handle c)))))

              (loop while (run-device) do
                   (log-for (trace) "Request queue device restarting."))
              (log-for (trace) "Queued request device exiting.")))))))


