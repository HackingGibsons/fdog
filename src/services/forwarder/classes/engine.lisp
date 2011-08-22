(in-package :fdog-forwarder)

;; Forwarder engine, that which makes the definitions dance
(defclass forwarder-engine ()
  ((forwarder :initarg :forwarder
              :accessor forwarder-engine-forwarder)
   (aliases :accessor forwarder-engine-aliases
            :initform nil
            :documentation "A cache of forwarder alias definitions.")
   (servers :initarg :servers
            :initform (forwarder-servers)
            :accessor forwarder-engine-servers)
   (bridges :initarg :bridges
            :initform ()
            :accessor forwarder-engine-bridges)
   (alias-endponits :accessor forwarder-engine-alias-endpoints
                    :initform nil)
   (endpoint :accessor forwarder-engine-endpoint))
  (:documentation "The engine that manages the forwarding of requests for an endpoint"))

;; Pretty printer
(defmethod print-object ((e forwarder-engine) s)
  (format s "#<Forwarder-Engine for(~A): ~A aliases>"
          (if (slot-boundp e 'forwarder) (fdog-forwarder-name (forwarder-engine-forwarder e)) "Undef")
          (length (forwarder-engine-aliases e))))
