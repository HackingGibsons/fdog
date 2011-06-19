(defpackage #:fdog-services
  (:use #:cl)
  (:export :init-services))
(in-package :fdog-services)

;; TODO: Services could specialize :afters on this instead of me
;;       piling up a bunch of services here
(defmethod init-services (&rest services)
  (fdog-control:init-control-interface))
