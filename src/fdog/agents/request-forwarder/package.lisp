(defpackage #:request-forwarder-agent
  (:documentation "A package for the agent that implements request/response
rewriting, routing and forwarding.")
  (:use :cl
        :log5)

  (:use :afdog
        :agent
        :request-processing-agent)

  (:import-from :arnesi
                :rcurry
                :when-bind)
  (:import-from :alexandria
                :appendf)

  (:export :request-forwarder-agent
           :push-state-signal
           :forwarder-endpoint
           :push-sock
           :sub-sock
           :sock-of
           :addr-of
           :push-ready
           :push-state
           :name
           :push-unready
           :deliver-request
           :deliver-response
           :delivery-faulure))

(in-package :request-forwarder-agent)
