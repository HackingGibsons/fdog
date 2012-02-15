(defpackage #:request-forwarder-agent
  (:documentation "A package for the agent that implements the HTTP API
for Afdog.")
  (:use :cl
        :log5)

  (:use :afdog
        :agent
        :request-processing-agent)

  (:import-from :arnesi
                :when-bind)
  (:import-from :alexandria
                :appendf)

  (:export :request-forwarder-agent))

(in-package :request-forwarder-agent)
