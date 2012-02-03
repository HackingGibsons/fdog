(defpackage #:api-agent
  (:use :cl)
  (:documentation "A package for the agent that implements the HTTP API
for Afdog.")
  (:use :afdog
        :agent
        :log5
        :request-processing-agent)
  (:export :api-agent))

(in-package :api-agent)
