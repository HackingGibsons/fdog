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
(defmethod request-handler :around ((agent request-forwarder-agent) organ req data)
  "Request storage hook."
  (declare (ignore data organ))
  (let ((request (call-next-method)))
    (log-for (warn request-storage) "TODO: Store the request: [~S]" (babel:octets-to-string (m2cl:request-serialize request)))))

(defmethod response-handler :after ((agent request-forwarder-agent) organ data)
  "Response storage hook."
  (let ((identifier (response-id data)))
    (log-for (warn request-storage) "TODO: Handle the response [~S] by storing it." (babel:octets-to-string data))))

(defmethod delivery-failure-handler ((agent request-forwarder-agent) organ req)
  (log-for (warn request-storage) "TODO: Queue the request: ~S" (m2cl:request-serialize req)))

