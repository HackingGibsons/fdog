(in-package :fdog-forwarder)

;; Request queuing machinery
(defun reconnect-redis-handler (c)
  (log-for (warn) "Reconnecting to Redis!!")
  (let ((reconnect (find-restart :reconnect)))
    (if reconnect
        (progn
          (log-for (warn) "Reconnect restart found")
          (invoke-restart reconnect))
        (progn
          (log-for (warn) "There is no reconnect restart")
          (error c)))))

(defvar *request-event-lock* (bt:make-lock "request-event-lock"))

(defmethod request-event-info ((endpoint forwarder-queue-endpoint) redis field &key (type :int))
  (log-for (warn) "Thread: ~A waiting on request-event-info lock" (bt:current-thread))
  (bt:with-lock-held (*request-event-lock*)
    (log-for (warn) "Thread: ~A holds the request-event-info lock" (bt:current-thread))
    (let ((val (redis:lred-hget redis (endpoint-queue-counter endpoint) field)))
      (ecase type
        (:int (parse-integer val))
        (:raw val)))))

(defmethod request-queue-event ((endpoint forwarder-queue-endpoint) redis (event symbol))
  (log-for (warn) "Thread: ~A waiting on request-queue-event lock" (bt:current-thread))
  (bt:with-lock-held (*request-event-lock*)
    (log-for (warn) "Thread: ~A holds the request-queue-event-lock" (bt:current-thread))
    (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
      (ecase event
        (:reset (log-for (trace) "Resetting inflight request counter.")
                (redis:lred-hmset redis (endpoint-queue-counter endpoint)
                                  :count 0
                                  :last-pop 0
                                  :last-sent 0)
                (redis:lred-set redis *total-queue-counter-key* 0))
        (:popped (log-for (trace) "Request popped from queue")
                 (redis:lred-multi redis)
                 (redis:lred-hincrby redis (endpoint-queue-counter endpoint) :count 1)
                 (redis:lred-incrby redis *total-queue-counter-key* 1)
                 (redis:lred-hset redis (endpoint-queue-counter endpoint) :last-pop (get-internal-real-time))
                 (redis:lred-publish redis (endpoint-queue-key endpoint) :popped)
                 (redis:lred-exec redis))
        (:sent (log-for (trace) "Request sent to handler.")
               (redis:lred-multi redis)
               (redis:lred-hincrby redis (endpoint-queue-counter endpoint) :count -1)
               (redis:lred-incrby redis *total-queue-counter-key* -1)
               (redis:lred-hset redis (endpoint-queue-counter endpoint) :last-sent (get-internal-real-time))
               (redis:lred-publish redis (endpoint-queue-key endpoint) :sent)
               (redis:lred-exec redis))))))

;; LOCKS
;; This meth, for one reason or another fails intermitently even with a lexically bound redis passed in :(
;; (unless (get 'request-queue-event 'lock)
;;   (setf (get 'request-queue-event 'lock) (bt:make-lock "request-queue-event")))

;; (unless (get 'request-event-info 'lock)
;;   (setf (get 'request-event-info 'lock) (bt:make-lock "request-event-info")))


;; TODO: better way to handle fdog-queue:
(defmethod endpoint-queue-key ((forwarder fdog-forwarder))
  (format nil "~A:~A:request-queue" "fdog-queue:" (fdog-forwarder-name forwarder)))

(defmethod endpoint-queue-key ((endpoint forwarder-queue-endpoint))
  (with-slots (queue-prefix) endpoint
    (let* ((name (fdog-forwarder-name (endpoint-engine endpoint)))
           (name (if (endpoint-alias endpoint)
                     (format nil "~A-alias-~A" name (fdog-forwarder-alias-name (endpoint-alias endpoint)))
                     name)))
      (format nil "~A:~A:request-queue" queue-prefix
              name))))

;; TODO: need a way to handle aliases?
(defmethod endpoint-queue-counter ((endpoint forwarder-queue-endpoint))
  (format nil "~A:counter" (endpoint-queue-key endpoint)))

(defmethod endpoint-queue-counter ((forwarder fdog-forwarder))
  (format nil "~A:counter" (fdog-forwarder-name forwarder)))

(defmethod request-queue-length ((forwarder fdog-forwarder))
  (redis:with-named-connection (redis :host *redis-host*
                                      :port *redis-port*)
   (parse-integer (or (redis:lred-hget redis (endpoint-queue-counter forwarder) :count) "0"))))

(defun total-request-queue-length ()
  (redis:with-named-connection (redis :host *redis-host*
                                      :port *redis-port*)
    (parse-integer (or (redis:lred-get redis *total-queue-counter-key*) "0"))))

(defun format-decoded-time ()
  "Returns the universal time formatted in YYYYMMDDHHmmss"
  (multiple-value-bind (seconds minutes hours date month year) (get-decoded-time)
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D" year month date hours minutes seconds)))

(defmethod endpoint-request-key ((endpoint forwarder-engine-endpoint) request)
  "Generate a key to store the given `request' under for the endpoint `endpoint'"
  (endpoint-key endpoint *request-prefix* request))

(defmethod endpoint-response-key ((endpoint forwarder-engine-endpoint) response)
  (endpoint-key endpoint *response-prefix* response))

(defmethod endpoint-key ((endpoint forwarder-engine-endpoint) prefix msg)
  (format nil "~A:~A:~A:~A:~A" prefix
          (fdog-forwarder-name (endpoint-engine endpoint))
          (crypto:byte-array-to-hex-string (crypto:digest-sequence :sha256 msg))
          (format-decoded-time)
          (get-internal-real-time)))

(defun endpoint-request-counter-key ()
  (endpoint-counter-key *request-prefix*))

(defun endpoint-response-counter-key ()
  (endpoint-counter-key *response-prefix*))

(defun endpoint-counter-key (prefix)
  (format nil "~A:~A" *counter-prefix* prefix))

(defun request-count (forwarder-name)
  "Retrieves the number of requests served for a given forwarder."
  (msg-count (endpoint-request-counter-key) forwarder-name))

(defun total-request-count ()
  "Retrieves the total number of requests served by fdog."
  (total-msg-count *total-request-counter-key*))

(defun response-count (forwarder-name)
  "Retrieves the number of responses served for a given forwarder."
  (msg-count (endpoint-response-counter-key) forwarder-name))

(defun total-response-count ()
  "Retrieves the total number of responses served by fdog."
  (total-msg-count *total-response-counter-key*))

(defun msg-count (key forwarder-name)
  (redis:with-named-connection (redis :host *redis-host*
                                      :port *redis-port*)
    (parse-integer (or (redis:lred-hget redis key forwarder-name) "0"))))

(defun total-msg-count (key)
  (redis:with-named-connection (redis :host *redis-host*
                                      :port *redis-port*)
    (parse-integer (or (redis:lred-get redis key) "0"))))

(defun average-response-time ()
  0)

(defmethod store-request ((endpoint forwarder-queue-endpoint) redis msg)
  "Store the request in redis and return a key that can be used to reffer to it."
  (let ((key (endpoint-request-key endpoint msg))
        (counter-key (endpoint-request-counter-key)))
    (store-msg endpoint redis key counter-key msg)
    (redis:lred-incrby redis *total-request-counter-key* 1)
    key))

(defmethod store-response ((endpoint forwarder-queue-endpoint) redis msg)
  "Store the response in redis and return a key that can be used to reffer to it."
  (let ((key (endpoint-response-key endpoint msg))
        (counter-key (endpoint-response-counter-key)))
    (store-msg endpoint redis key counter-key msg)
    (redis:lred-incrby redis *total-response-counter-key* 1)
    key))

(defmethod store-msg ((endpoint forwarder-queue-endpoint) redis key counter-key msg)
  (log-for (trace) "Stored request for ~A" (fdog-forwarder-name (endpoint-engine endpoint)))
  (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
    (redis:lred-hset redis key :body (babel:octets-to-string msg))
    (redis:lred-hincrby redis counter-key (fdog-forwarder-name (endpoint-engine endpoint)) 1)))

(defmethod queue-request ((endpoint forwarder-queue-endpoint) redis msg)
  "Enqueue message on the endpoint to the current connected redis instance."
  (let ((request-key (store-request endpoint redis msg))
        (queue-key (endpoint-queue-key endpoint)))
    ;; TODO: Make sure this doesn't spin wildly if the server is outright down
    (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
      (redis:lred-multi redis)
      (redis:lred-lpush redis queue-key request-key)
      #|Trim|#
      (redis:lred-publish redis (endpoint-queue-key endpoint) :pushed)
      (redis:lred-exec redis))
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
      (let ((msg (make-instance 'zmq:msg)))
        (redis:with-named-connection (redis :host *redis-host*
                                            :port *redis-port*)
          (labels ((run-once ()
                     (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
                       (log-for (trace) "Waiting for message to queue for endpoint: ~A" endpoint)
                       (log-for (trace) "Recv() result for endpoint->queue[~A]: ~A" endpoint
                                (zmq:recv! (endpoint-queue-sock endpoint) msg))
                       (log-for (trace) "Queueing message.")
                       (prog1 (queue-request endpoint redis (zmq:msg-data-as-array msg))
                         (log-for (trace) "Request queued for endpoint: ~A" endpoint)))
                     :always-run)

                   (handle-condition (c)
                     (or (when (or :always-run ;; Again, spin good, silent failure bad.
                                   (= (sb-alien:get-errno) sb-posix:eintr))
                           (log-for (warn) "Queue device restarting on endpoint: ~A" endpoint)
                           t)
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
      (let ((prev-count (redis:with-named-connection (redis :host *redis-host*
                                                            :port *redis-port*)
                          (request-event-info endpoint redis :count)))
            cur-count)
        (redis:with-named-connection (redis :host *redis-host*
                                            :port *redis-port*)
          (zmq:with-socket (forward-sock (endpoint-context endpoint) zmq:push)
            (maybe-linger-socket forward-sock)
            (zmq:connect forward-sock (endpoint-proxy-addr endpoint))
            (labels ((run-once ()
                       (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
                         (redis:lred-watch redis (endpoint-queue-counter endpoint))
                         (setf cur-count (request-event-info endpoint redis :count))
                         (if (and (> cur-count 0)
                                  (> cur-count prev-count))
                             (progn
                               (log-for (warn) "Requests are being backlogged against a blocked writer! Now: ~A" cur-count)

                               (redis:lred-multi redis)
                               (redis:lred-qsubscribe redis (endpoint-queue-key endpoint))
                               (unless (redis:lred-exec redis)
                                 (log-for (warn) "Inflight count changed since we last checked, going again")
                                 (return-from run-once :again))

                               (do ((msg (redis:expect redis :multi) (redis:expect redis :multi)))
                                   ((string-equal (third msg) :sent))
                                 (log-for (warn) "Waiting for next send message to resume queue pickups.."))
                               (log-for (warn) "Writing to the client has resumed, resuming queue reads.")
                               (log-for (warn) "Unsubscribing..")
                               (redis:lred-unsubscribe redis)
                               (log-for (warn) "Going for next request."))

                             (redis:lred-unwatch redis))

                         (let* ((req-key (car (last (redis:lred-brpop redis (endpoint-queue-key endpoint) 0))))
                                (request (and req-key (redis:lred-hget redis req-key :body))))
                           (log-for (trace) "Got request: ~A" req-key)
                           (log-for (trace) "Expiring request in ~A seconds." (queue-endpoint-request-linger endpoint))
                           (redis:lred-expire redis req-key (queue-endpoint-request-linger endpoint))

                           (and
                            (setf prev-count cur-count)
                            (request-queue-event endpoint redis :popped)
                            (= 0 (zmq:send! forward-sock (make-instance 'zmq:msg :data request)))))))


                     (handle-condition (c)
                       (if (= (sb-alien:get-errno) sb-posix:eintr)
                           t
                           (prog1 :always-run ;; Search for ":always-run" for the explanation
                             (log-for (warn) "Queue request writer device exited with condition: ~A" c)
                             (error c))))

                     (run-device ()
                       (handler-case (run-once)
                         (simple-error (c) (handle-condition c)))))

              (loop while (run-device) do ':nothing)))))))

(defmethod make-response-logging-device ((endpoint forwarder-queue-endpoint))
  #'(lambda ()
      (with-slots (context response-proxy-addr) endpoint
        (zmq:with-socket (socket context zmq:sub)
          (zmq:connect socket response-proxy-addr)
          (zmq:setsockopt socket zmq:subscribe "")
          (redis:with-named-connection (redis :host *redis-host*
                                              :port *redis-port*)
            (labels
                ((run-once ()
                   (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
                     (let ((response (make-instance 'zmq:msg)))
                       (zmq:recv! socket response)
                       ;; Store in redis without queueing it,
                       ;; and expire it after the response linger time
                       (let ((key (store-response endpoint redis (zmq:msg-data-as-array response))))
                         (log-for (trace) "Wrote response key to redis: ~A" key)
                         (log-for (trace) "Expiring response in ~A seconds." (queue-endpoint-response-linger endpoint))
                         (redis:lred-expire redis key (queue-endpoint-response-linger endpoint))))
                     t))

                 (run-device ()
                   (handler-case (run-once)
                     (simple-error (c) t))))

              (loop while (run-device) do (run-device))))))))

(defmethod engine-endpoint-start :after ((endpoint forwarder-queue-endpoint))
  (redis:with-named-connection (redis :host *redis-host*
                                      :port *redis-port*)
    (request-queue-event endpoint redis :reset))
  (with-slots (request-write-device request-queue-device response-logging-device) endpoint
    (setf request-queue-device
          (make-thread (make-request-queuer-device endpoint)
                       :name (format nil "engine-request-queue-request-queue-device-~A"
                                     (fdog-forwarder-name (endpoint-engine endpoint))))

          request-write-device
          (make-thread (make-request-writer-device endpoint)
                       :name (format nil "engine-request-queue-request-write-device-~A"
                                     (fdog-forwarder-name (endpoint-engine endpoint))))
          response-logging-device
          (make-thread (make-response-logging-device endpoint)
                       :name (format nil "engine-endpoint-device-response-logging-~A"
                                     (fdog-forwarder-name (endpoint-engine endpoint)))))))

(defmethod engine-endpoint-stop :before ((endpoint forwarder-queue-endpoint))
  (log-for (trace) "Stopping the queue request writer thread for: ~A" endpoint)
  (with-slots (request-write-device request-queue-device response-logging-device) endpoint
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
    (and response-logging-device
         (threadp response-logging-device)
         (thread-alive-p response-logging-device)
         (destroy-thread response-logging-device)
         (log-for (trace) "Killed response logger for: ~A" endpoint))
    (setf request-queue-device nil
          request-write-device nil
          response-logging-device nil)))

(defmethod request-forwarding-address ((endpoint forwarder-queue-endpoint))
  (log-for (trace) "Serving a queue address to forward requests to.")
  (endpoint-queue-addr endpoint))

(defmethod make-request-device ((endpoint forwarder-queue-endpoint))
  #'(lambda ()
      (log-for (trace) "Starting queued proxy request device")
      (let (last-message)
        (redis:with-named-connection (redis :host *redis-host*
                                            :port *redis-port*)
            (labels ((run-once ()
                       (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
                         (let ((msg (make-instance 'zmq:msg))
                               recv send)
                           (log-for (trace) "Reading queued request")
                           (unless last-message
                             (setf recv (zmq:recv! (endpoint-proxy-sock endpoint) msg))
                             (and (= recv 0)
                                  (setf last-message msg)))
                           (log-for (trace) "Read queued request: ~A" recv)
                           (log-for (trace) "Sending queued request.")
                           (and last-message
                                (setf send (zmq:send! (endpoint-request-sock endpoint) last-message)))
                           (when (= send 0)
                             (setf last-message nil)
                             (request-queue-event endpoint redis :sent)
                             (log-for (trace) "Sent queued request: ~A" send))
                           (not last-message)))
                       :always-run)

                     (handle (c)
                       (log-for (warn) "Queued request device condition: ~A" c)
                       ; (= (sb-alien:get-errno) sb-posix:eintr)
                       :always-run)

                     (run-device ()
                       (handler-case (run-once)
                         (simple-error (c) (handle c)))))

              (loop while (run-device) do
                   (log-for (trace) "Request queue device restarting."))
              (log-for (trace) "Queued request device exiting."))))))


