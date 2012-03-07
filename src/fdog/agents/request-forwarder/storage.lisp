(in-package :request-forwarder-agent)
(defcategory request-storage)

;; Knobs
(defvar *expire-after* 600
  "The interval of time requests and responses persist in the database after they are written.")

;; Helpers
(defun response-id (data)
  "Read the id of a response to match it up with a request produced by the system."
  (let* ((response (babel:octets-to-string data))
         (target-end (position #\Space response)))
    (and (numberp target-end)
         (not (zerop target-end))
         (second (ppcre:split "--id-([^\s]+)" response :end target-end :with-registers-p t :omit-unmatched-p t)))))

;; Hooks
(defmethod deliver-request :before ((endpoint forwarder-endpoint) (req m2cl:request))
  "Request storage hook."
  (store-request endpoint req))

(defmethod deliver-request :after ((endpoint forwarder-endpoint) (req m2cl:request))
  "Request expire hook"
  (expire-request endpoint req))

(defmethod deliver-response :around ((endpoint forwarder-endpoint) data)
  "Response storage hook."
  (unwind-protect (call-next-method)
    (store-response endpoint data)))

(defmethod push-state-signal :after (agent organ (endpoint forwarder-endpoint))
  (when (push-ready-p endpoint)
    (update-queue-count endpoint)))

(defmethod endpoint-write-callback :after ((endpoint forwarder-endpoint))
  (unless (zerop (queue-count endpoint))
    (deliver-from-queue endpoint)))

(defmethod delivery-failure-handler ((agent request-forwarder-agent) organ (endpoint forwarder-endpoint) req)
  "Request queue hook."
  (queue-request endpoint req))

;; Action methods.
(defgeneric store-request (endpoint request)
  (:documentation "Store the request data.")

  (:method ((endpoint forwarder-endpoint) req)
    (let* ((id (m2cl:request-header req *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (key (prefixed-key (agent endpoint) :request id)))
      (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
        (redis:with-pipelining
          (redis:red-multi)
          (redis:red-hsetnx key :data (babel:octets-to-string (m2cl:request-serialize req)))
          (redis:red-hsetnx key :stored (get-universal-time))
          (redis:red-exec))))))

(defgeneric expire-request (endpoint request)
  (:documentation "Set the request to expire after a given time.")

  (:method ((endpoint forwarder-endpoint) req)
    (let* ((id (m2cl:request-header req *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (key (prefixed-key (agent endpoint) :request id))
           (expireset-key (prefixed-key (agent endpoint) "requests" "expiring")))

      (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
        (redis:with-pipelining
          (redis:red-multi)
          (redis:red-zadd expireset-key (+ (get-universal-time) *expire-after*) key)
          (redis:red-expire key *expire-after*)
          (redis:red-expire expireset-key *expire-after*)
          (redis:red-zremrangebyscore expireset-key "-inf" (get-universal-time))
          (redis:red-exec))))))

(defgeneric store-response (endpoint response)
  (:documentation "Store the response data in a way that can be found through
the request data.")

  (:method ((endpoint forwarder-endpoint) data)
    (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
      (let* ((id (response-id data))
             (key (prefixed-key (agent endpoint) :response id))
             (expireset-key (prefixed-key (agent endpoint) "responses" "expiring"))
             (reply-count (redis:red-hincrby key :count 1))
             (specific-key (format nil "~A:~A" key reply-count)))

        (redis:with-pipelining
          (redis:red-hmset specific-key
                           :data (babel:octets-to-string data)
                           :stored (get-universal-time))
          (redis:red-multi)
          (redis:red-zadd expireset-key (+ (get-universal-time) *expire-after*) key)
          (redis:red-expire key *expire-after*)
          (redis:red-expire specific-key *expire-after*)
          (redis:red-expire expireset-key *expire-after*)
          (redis:red-zremrangebyscore expireset-key "-inf" (get-universal-time))
          (redis:red-exec))))))

(defgeneric queue-request (endpoint request)
  (:documentation "Append the request to the queue of requests handled by `agent'")

  (:method ((endpoint forwarder-endpoint) request)
    (let* ((id (m2cl:request-header request *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (request-key (prefixed-key (agent endpoint) :request id))
           (queue-key (prefixed-key (agent endpoint) :request :queue))
           (queues-key (prefixed-key (agent endpoint) :queues)))

      (incf (queue-count endpoint))

      (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
        (redis:with-pipelining
          (redis:red-multi)
          ;; Store the name of the current queue
          (redis:red-sadd queues-key queue-key)
          (redis:red-lpush queue-key request-key)
          (redis:red-exec))))))

(defgeneric deliver-from-queue (endpoint)
  (:documentation "Try to deliver a request from the queue and update the queue count.")

  (:method ((endpoint forwarder-endpoint))
    (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
      (let* ((queue-key (prefixed-key (agent endpoint) :request :queue))
             (request-key (redis:red-rpop queue-key))
             (request (and request-key
                           (m2cl:request-parse (babel:string-to-octets (redis:red-hget request-key :data))))))

        (when request
          (handler-case (deliver-request endpoint request)
              (delivery-failure ()
                (delivery-failure-handler (agent endpoint) (organ endpoint) endpoint request)))))))

  (:method :after ((endpoint forwarder-endpoint))
    (update-queue-count endpoint)))

