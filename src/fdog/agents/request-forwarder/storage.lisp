(in-package :request-forwarder-agent)
(defcategory request-storage)

;; Knobs
(defvar *expire-after* 60
  "The interval of time requests and responses persist in the database after they are written.")

;; Helpers
(defun reconnect-redis-handler (c)
  "Reconnect handler for a redis connection"
  (log-for (warn) "Reconnecting to Redis!!")
  (let ((reconnect (find-restart :reconnect)))
    (if reconnect
        (progn
          (log-for (warn) "Reconnect restart found")
          (invoke-restart reconnect))
        (progn
          (log-for (warn) "There is no reconnect restart")
          (error c)))))

(defun prefixed-key (agent id suffix &rest suffix-args)
  "Generate a key usable for storage using the `agent', `request', `id' and `suffix'.
`suffix-args' will be applied to suffix as in `format'"
  (format nil "~A:~A:~A" (forwarder agent) (apply #'format nil (typecase suffix
                                                                 (string suffix)
                                                                 (otherwise (prin1-to-string suffix)))
                                                  suffix-args) id))

(defun response-id (data)
  "Read the id of a response to match it up with a request produced by the system."
  (let* ((response (babel:octets-to-string data))
         (target-end (position #\Space response)))
    (and (numberp target-end)
         (not (zerop target-end))
         (second (ppcre:split "--id-([^\s]+)" response :end target-end :with-registers-p t :omit-unmatched-p t)))))

;; Hooks
(defmethod deliver-request :around (endpoint req)
  "Request storage and expiry hook."
  (flet ((do-store (&optional c)
           (declare (ignore c))
           (store-request (agent endpoint) req)))
    (handler-bind ((t #'do-store))
      (prog1 (call-next-method)
        (do-store)
        (expire-request (agent endpoint) req)))))

(defmethod response-handler :after ((agent request-forwarder-agent) organ data)
  "Response storage hook."
  (store-response agent data))

(defmethod delivery-failure-handler ((agent request-forwarder-agent) organ req)
  "Request queue hook."
  (queue-request agent req))

;; Action methods.
(defgeneric store-request (agent request)
  (:documentation "Store the request data.")

  (:method ((agent request-forwarder-agent) req)
    (let* ((id (m2cl:request-header req *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (key (prefixed-key agent id :request)))
      (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
        (redis:with-pipelining
          (redis:red-multi)
          (redis:red-hsetnx key :data (babel:octets-to-string (m2cl:request-serialize req)))
          (redis:red-hsetnx key :stored (get-universal-time))
          (redis:red-exec))))))

(defgeneric expire-request (agent request)
  (:documentation "Set the request to expire after a given time.")

  (:method ((agent request-forwarder-agent) req)
    (let* ((id (m2cl:request-header req *request-id-header* (format nil "UNKNOWN-~A" (uuid:make-v4-uuid))))
           (key (prefixed-key agent id :request)))

      (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))
        (redis:red-expire key *expire-after*)))))

(defgeneric store-response (agent response)
  (:documentation "Store the response data in a way that can be found through
the request data.")

  (:method ((agent request-forwarder-agent) data)
    (let ((identifier (response-id data)))
      (handler-bind ((redis:redis-connection-error #'reconnect-redis-handler))

        (log-for (warn request-storage) "TODO: Handle the response [~S] == [~S] by storing it." identifier (babel:octets-to-string data))))))

(defgeneric queue-request (agent request)
  (:documentation "Append the request to the queue of requests handled by `agent'")

  (:method ((agent request-forwarder-agent) request)
    (log-for (warn request-storage) "TODO: Queue the request: [~S]" (babel:octets-to-string (m2cl:request-serialize request)))))
