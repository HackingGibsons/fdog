(defpackage #:fdog-services
  (:use #:cl)
  (:export :init-services))
(in-package :fdog-services)

(defmethod init-services (&rest services)
  (fdog-control:init-control-interface)
  (fdog-forwarder:init-forwarders))
