(in-package :request-forwarder-agent)

(defmethod disconnect-handler ((agent request-forwarder-agent) organ req data)
  (log-for (trace request-forwarder-agent) "R-F-A: Disconnect: ~A" req))

(defgeneric delivery-failure-handler (agent organ request)
  (:documentation "A hook to handle the failure to deliver a request
that comes in from the outside.")

  (:method ((agent standard-agent) organ req)
    "Default handler is to fail with a 503"
    (log-for (trace request-forwarder-agent) "R-F-A: Request failed to deliver: ~A" req)
    (let ((organ (find-organ agent :requesticle)))
      (and organ
           (http-dog:with-chunked-stream-reply ((handler organ) req s
                                                :code 503 :status "SERVICE UNAVAILABLE"
                                                :headers ((http-dog:header-json-type)))
             (json:encode-json-plist `(:error "Upstream delivery failure. No recovery options present.") s))))))

(defgeneric response-handler (agent organ data)
  (:documentation "Trigger for response handling.")
  (:method (agent organ data) nil))

(defmethod request-handler ((agent request-forwarder-agent) organ req data)
  "Handle the transformation and delivery of a request."
  (declare (ignorable data))
  (log-for (trace request-forwarder-agent) "R-F-A: Request: ~A" req)
  (flet ((apply-transformation (request transform)
           (log-for (trace request-forwarder-agent) "Transforming ~A with ~A" request transform)
           (typecase transform
               (keyword (agent-request-transform agent transform req))
               (otherwise (funcall transform request)))))

    (let* ((sock-pocket (find-organ agent :sock-pocket))
           (request (reduce #'apply-transformation (transforms agent)
                            :initial-value req))
           (endpoint (client-endpoint sock-pocket :default)))

      (prog1 request
        (if (push-ready-p endpoint)
            (handler-case (deliver-request endpoint request)
              (delivery-failure () (delivery-failure-handler agent organ request)))
            (delivery-failure-handler agent organ request))))))
