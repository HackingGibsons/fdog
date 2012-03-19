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
  (:import-from :mongrel2-agent
                :*control-port*)
  (:export :api-agent
           :forwarders
           :endpoints
           :callback
           :register-callback
           :*control-server*
           :*api-handler*))

(in-package :api-agent)
