(defpackage #:fdog
  (:use :afdog-hypervisor-agent)
  (:use :mongrel2-agent)
  (:use :request-processing-agent)
  (:use :api-agent)
  (:use :forwarder-agent)
  (:use :request-forwarder-agent)
  ;; req-proc and forwarder both export handler-name
  (:shadow :handler-name)
  (:export :afdog-hypervisor-agent
           :mongrel2-agent
           :*control-port*
           :api-agent
           :request-processing-agent
           :forwarder-agent))

(in-package :fdog)
