(defpackage #:agent
  (:use #:cl)
  (:use #:log5)
  (:use :afdog)
  (:export :standard-agent
           :defbehavior
           :behavior
           :send-message
           :standard-manager-agent
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
           :agent-info
           :agent-provides
           :watching
           :organ-tag
           :running-p
           :parse-message
           :read-message
           :prepare-message
           :with-agent-conversation
           :discover-agents-on-host
           :local-ipc-addr
           :agent-event-count
           :agent-context
           :agent-organs
           :run-agent
           :standard-state-machine
           :state-machine
           :state
           :machine
           :defstate
           :standard-state-machine-event
           :next-event
           :suicide
           :agent-head
           :heard-message))

(in-package :agent)

(defvar *spawner* :thread
  "The type of maker to use to spawn agents. Define it in another package
to specify how agents in that package should spawn.")
