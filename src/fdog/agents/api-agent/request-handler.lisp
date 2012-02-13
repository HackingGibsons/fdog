(in-package :api-agent)

(defmethod request-handler ((agent api-agent) organ req raw)
  "Request handler."
  (api-app:api agent organ (handler agent) req raw))

