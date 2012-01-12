(defpackage #:request-processing-agent
  (:use :cl)
  (:documentation "A package for an agent that can seeks out, accept and act on
Mongrel2 ZeroMQ messages")
  (:use #:afdog
        #:agent
        #:log5)
  (:import-from :arnesi
                :it
                :aif)
  (:export :request-processing-agent))

(in-package :request-processing-agent)
