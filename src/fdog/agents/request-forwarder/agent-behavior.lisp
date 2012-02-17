(in-package :request-forwarder-agent)

(defmethod disconnect-handler ((agent request-forwarder-agent) organ req data)
  (log-for (trace request-forwarder-agent) "R-F-A: Disconnect: ~A" req))

(defmethod request-handler ((agent request-forwarder-agent) organ req data)
  "Handle the transformation and delivery of a request."
  (declare (ignorable data))
  (log-for (trace request-forwarder-agent) "R-F-A: Request: ~A" req)
  (flet ((apply-transformation (request transform)
           (log-for (trace request-forwarder-agent) "Transforming ~A with ~A" acc b)
           (funcall transform request))

         (match-endpoint (selected consider request)
           (if (wants-request-p consider request)
               consider
               selected)))

    (let* ((sock-pocket (find-organ agent :sock-pocket))
           (request (reduce #'apply-transformation (transforms agent)
                            :initial-value req))
           ;; TODO: Don't just reach in and grab default
           ;;       search for the "correct" endpoint to use
           (endpoint (client-endpoint sock-pocket :default)))

      (log-for (trace request-forwarder-agent)
               "Sockpock: ~A" sock-pocket)
      (log-for (trace request-forwarder-agent)
               "Endpoint: ~A" endpoint)
      (log-for (trace request-forwarder-agent)
               "Transformed request: ~A"
               (babel:octets-to-string (m2cl:request-serialize req)))

      (deliver-request endpoint request))))
