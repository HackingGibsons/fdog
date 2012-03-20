(in-package :api-agent)

(defmethod request-handler ((agent api-mixin) organ req raw)
  "Request handler."
  (api-app:api agent organ (handler agent) req raw))

