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
  (:export :api-agent
           :forwarders
           :callback
           :register-callback
           :*control-server*
           :*api-handler*))

(in-package :api-agent)
