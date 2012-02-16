(in-package :request-forwarder-agent)

(defmethod disconnect-handler ((agent request-forwarder-agent) organ req data)
  (log-for (trace request-forwarder-agent) "R-F-A: Disconnect: ~A" req))

(defmethod request-handler ((agent request-forwarder-agent) organ req data)
  (log-for (trace request-forwarder-agent) "R-F-A: Request: ~A" req))
