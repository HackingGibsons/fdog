(in-package :request-forwarder-agent)
(defcategory request-storage)

;; Helpers
(defun response-id (data)
  "Read the id of a response to match it up with a request produced by the system."
  (let* ((response (babel:octets-to-string data))
         (target-end (position #\Space response)))
    (and (numberp target-end)
         (not (zerop target-end))
         (second (ppcre:split "--id-([^\s]+)" response :end target-end :with-registers-p t :omit-unmatched-p t)))))

;; Hooks
(defmethod deliver-request :around (endpoint req)
  "Request storage hook."
  (unwind-protect (call-next-method)
    (store-request (agent endpoint) req)))

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
    (log-for (warn request-storage) "TODO: Store the request data of: [~S]." (babel:octets-to-string (m2cl:request-serialize req)))))

(defgeneric store-response (agent response)
  (:documentation "Store the response data in a way that can be found through
the request data.")

  (:method ((agent request-forwarder-agent) data)
    (let ((identifier (response-id data)))
      (log-for (warn request-storage) "TODO: Handle the response [~S] == [~S] by storing it." identifier (babel:octets-to-string data)))))

(defgeneric queue-request (agent request)
  (:documentation "Append the request to the queue of requests handled by `agent'")

  (:method ((agent request-forwarder-agent) request)
    (log-for (warn request-storage) "TODO: Queue the request: [~S]" (babel:octets-to-string (m2cl:request-serialize request)))))


