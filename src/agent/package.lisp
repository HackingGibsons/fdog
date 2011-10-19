(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog)
  (:export :standard-agent
           :agent-special-event
           :agent-event-count
           :agent-context
           :agent-organs
           :run-agent))

(in-package :agent)
