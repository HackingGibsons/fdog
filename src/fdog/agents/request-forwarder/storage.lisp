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
  ;; TODO better place for this (this file concerns redis)
  (validate-request endpoint req)
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

;; TODO better place
(defgeneric validate-request (endpoint request)
  (:documentation "Validate the request against Accounts.")

  (:method ((endpoint forwarder-endpoint) (req m2cl:request))
    ;; TODO ignore in development
    ;; specialize on an environment?

    ;; TODO does fdog have its own api key?
    ;; TODO keep that out of version control
    ;; TODO where do i get the service name?
    (let* ((api-key (m2cl:request-header req *api-key-header*))
           (service nil)
           (content (json:encode-json-alist-to-string `((:api--key . ,api-key) (:service . ,service))))
           (args `(:method :POST :content-type "application/json" :content content :additional-headers ((,*api-key-header* . ,*agent-api-key*))))))

    ;; TODO REDIS see if key is valid and cached in redis
    ;; and when that's done move the content/args into an inner let
    ;; block

    ;; TODO how to get accounts url?
    ;; TODO timeout - fail and 504
    (multiple-value-bind (response status-code headers uri stream must-close reason-phrase)
        (apply 'drakma:http-request *accounts-url* args)
      (when must-close (close stream))

      (cond
        ((eql status-code 401)
         ;; TODO raise a 401 condition
         ;; TODO REDIS cache bad responses too?
         )
        ((eql status-code 200)
         ;; TODO REDIS cache in redis, set TTL
         )
        ))))

;; Action methods.
(defgeneric store-request (endpoint request)
  (:documentation "Store the request data.")

  (:method ((endpoint forwarder-endpoint) req)
    (let* ((id (m2cl:request-header req *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (key (prefixed-key (agent endpoint) :request id)))
      (with-agent-redis ((agent endpoint))
        (redis:with-pipelining
          (redis:red-multi)
          (redis:red-hsetnx key :data (babel:octets-to-string (m2cl:request-serialize req)))
          (redis:red-hsetnx key :stored (timestamp (agent endpoint)))
          (redis:red-exec))))))

(defgeneric expire-request (endpoint request)
  (:documentation "Set the request to expire after a given time.")

  (:method ((endpoint forwarder-endpoint) req)
    (let* ((id (m2cl:request-header req *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (key (prefixed-key (agent endpoint) :request id))
           (expireset-key (prefixed-key (agent endpoint) :requests :expiring)))

      (with-agent-redis ((agent endpoint))
        (redis:with-pipelining
          (redis:red-multi)
          (redis:red-hsetnx key :delivered (timestamp (agent endpoint)))
          (redis:red-zadd expireset-key (+ (timestamp (agent endpoint)) *expire-after*) key)
          (redis:red-expire key *expire-after*)
          (redis:red-expire expireset-key *expire-after*)
          (redis:red-zremrangebyscore expireset-key "-inf" (timestamp (agent endpoint)))
          (redis:red-exec))))))

(defgeneric store-response (endpoint response)
  (:documentation "Store the response data in a way that can be found through
the request data.")

  (:method ((endpoint forwarder-endpoint) data)
    (with-agent-redis ((agent endpoint))
      (let* ((id (response-id data))
             (key (prefixed-key (agent endpoint) :response id))
             (request-key (prefixed-key (agent endpoint) :request id))
             (expireset-key (prefixed-key (agent endpoint) :responses :expiring))

             (request-delivered-at (parse-integer (or (redis:red-hget request-key :delivered) "0")))
             (reply-count (parse-integer (first (alexandria:last-elt
                                                 ;; To keep the TTL atomic
                                                 (redis:with-pipelining
                                                   (redis:red-multi)
                                                   (redis:red-hincrby key :count 1)
                                                   (redis:red-expire key *expire-after*)
                                                   (redis:red-exec))))))
             (specific-key (format nil "~A:~A" key reply-count)))

        (redis:with-pipelining
          (redis:red-multi)
          (redis:red-hmset specific-key
                           :data (babel:octets-to-string data)
                           :stored (timestamp (agent endpoint)))

          (redis:red-hsetnx request-key :latency (- (timestamp (agent endpoint)) request-delivered-at))
          (redis:red-expire request-key *expire-after*) ;; Since we just violated the TTL

          (redis:red-hsetnx key :first-reply (timestamp (agent endpoint)))
          (redis:red-zadd expireset-key (+ (timestamp (agent endpoint)) *expire-after*) key)
          (redis:red-expire key *expire-after*)
          (redis:red-expire specific-key *expire-after*)
          (redis:red-expire expireset-key *expire-after*)
          (redis:red-zremrangebyscore expireset-key "-inf" (timestamp (agent endpoint)))
          (redis:red-exec))))))

(defgeneric queue-request (endpoint request)
  (:documentation "Append the request to the queue of requests handled by `agent'")

  (:method ((endpoint forwarder-endpoint) request)
    (let* ((id (m2cl:request-header request *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (request-key (prefixed-key (agent endpoint) :request id))
           (queue-key (queue-key endpoint))
           (queues-key (prefixed-key (agent endpoint) :queues)))

      (incf (queue-count endpoint))

      (with-agent-redis ((agent endpoint))
        (redis:with-pipelining
          (redis:red-multi)
          ;; Store the name of the current queue
          (redis:red-sadd queues-key queue-key)
          (redis:red-lpush queue-key request-key)
          (redis:red-exec))))))

(defgeneric deliver-from-queue (endpoint)
  (:documentation "Try to deliver a request from the queue and update the queue count.")

  (:method ((endpoint forwarder-endpoint))
    (with-agent-redis ((agent endpoint))
      (let* ((queue-key (queue-key endpoint))
             (request-key (redis:red-rpop queue-key))
             (request (and request-key (redis:red-hget request-key :data)))
             (request (m2cl:request-parse (babel:string-to-octets request))))

        (when request
          (handler-case (deliver-request endpoint request)
              (delivery-failure ()
                (delivery-failure-handler (agent endpoint) (organ endpoint) endpoint request))))))

    (update-queue-count endpoint)))
