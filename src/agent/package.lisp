(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog)
  (:export :standard-agent
           :defbehavior
           :behavior
           :send-message
           :standard-leaf-agent
           :standard-hypervisor-agent
           :find-organ
           :exec-runner
           :init-forms
           :organ
           :eye
           :organ-agent
           :watch-when-told
           :start
           :stop
           :make-runner
           :*spawner*
           :behaviors
           :behavior-organ
           :organ-incoming-sock
           :organ-outgoing-sock
           :agent-special-event
           :watching
           :organ-tag
           :running-p
           :parse-message
           :read-message
           :prepare-message
           :local-ipc-addr
           :agent-event-count
           :agent-context
           :agent-organs
           :run-agent
           :standard-state-machine
           :next-event
           :suicide))

(in-package :agent)

(defparameter *spawner* :thread
  "The type of maker to use to spawn agents. Define it in another package
to specify how agents in that package should spawn.")

