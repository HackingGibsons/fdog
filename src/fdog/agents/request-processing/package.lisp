(defpackage #:request-processing-agent
  (:use :cl)
  (:documentation "A package for an agent that can seeks out, accept and act on
Mongrel2 ZeroMQ messages")
  (:use #:afdog
        #:agent
        #:log5)
  (:import-from :arnesi
                :it
                :awhen
                :aif)
  (:export :request-processing-agent
           :agent-requesticle
           :request-handler
           :handler-name
           :handler))

(in-package :request-processing-agent)
