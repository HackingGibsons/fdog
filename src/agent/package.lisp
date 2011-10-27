(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog)
  (:export :standard-agent
           :agent-special-event
           :agent-event-count
           :agent-context
           :agent-organs
           :run-agent
           :next-event
           :suicide))

(in-package :agent)

(defparameter *spawner* :thread
  "The type of maker to use to spawn agents. Define it in another package
to specify how agents in that package should spawn.")

