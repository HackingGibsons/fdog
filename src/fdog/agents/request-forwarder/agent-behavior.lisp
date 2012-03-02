(in-package :request-forwarder-agent)

(defmethod disconnect-handler ((agent request-forwarder-agent) organ req data)
  (log-for (trace request-forwarder-agent) "R-F-A: Disconnect: ~A" req))

(defmethod delivery-failure-handler ((agent request-forwarder-agent) (organ agent-requesticle) req)
  (log-for (trace request-forwarder-agent) "R-F-A: Request failed to deliver: ~A" req)
  (http-dog:with-chunked-stream-reply ((handler organ) req s
                                       :code 503 :status "SERVICE UNAVAILABLE"
                                       :headers ((http-dog:header-json-type)))
    (json:encode-json-plist `(:error "Upstream delivery failure. No recovery options present.")
                            s)))

(defmethod request-handler ((agent request-forwarder-agent) organ req data)
  "Handle the transformation and delivery of a request."
  (declare (ignorable data))
  (log-for (trace request-forwarder-agent) "R-F-A: Request: ~A" req)
  (flet ((apply-transformation (request transform)
           (log-for (trace request-forwarder-agent) "Transforming ~A with ~A" request transform)
           (if (keywordp transform)
               (agent-request-transform agent transform req)
               (funcall transform request))))

    (let* ((sock-pocket (find-organ agent :sock-pocket))
           (request (reduce #'apply-transformation (transforms agent)
                            :initial-value req))
           (endpoint (client-endpoint sock-pocket :default)))

      (log-for (trace request-forwarder-agent)
               "Sockpock: ~A" sock-pocket)
      (log-for (trace request-forwarder-agent)
               "Endpoint: ~A" endpoint)
      (log-for (trace request-forwarder-agent)
               "Transformed request: ~A"
               (babel:octets-to-string (m2cl:request-serialize req)))

      (handler-case (deliver-request endpoint request)
        (delivery-failure ()
          (delivery-failure-handler agent organ request))))))
