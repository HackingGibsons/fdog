(in-package :api-app)

(defmethod find-forwarder ((agent api-agent) name)
  "Finds a forwarder by name"
  (assoc name (forwarders agent) :test #'string=))
