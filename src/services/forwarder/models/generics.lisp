(in-package :fdog-forwarder)

;; Some generics
(defgeneric forwarder-listen-on (f)
  (:documentation "Generic accesor for the `listen-on' slot")
  (:method ((f fdog-forwarder)) (fdog-forwarder-listen-on f))
  (:method ((f fdog-forwarder-alias)) (fdog-forwarder-alias-listen-on f)))

(defgeneric forwarder-forward-to (f)
  (:documentation "Generic accessor for the `forward-to' slot")
  (:method ((f fdog-forwarder)) (fdog-forwarder-forward-to f))
  (:method ((f fdog-forwarder-alias)) (fdog-forwarder-alias-forward-to f)))

