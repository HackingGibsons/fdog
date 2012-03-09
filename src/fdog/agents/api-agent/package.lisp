(defpackage #:api-agent
  (:use :cl)
  (:documentation "A package for the agent that implements the HTTP API
for Afdog.")
  (:use :afdog
        :agent
        :log5
        :request-processing-agent)
  (:import-from :alexandria
                :appendf)
  (:import-from :arnesi
                :when-bind)
  (:export :api-agent
           :forwarders
           :endpoints
           :callback
           :register-callback
           :*control-server*
           :*api-handler*))

(in-package :api-agent)
