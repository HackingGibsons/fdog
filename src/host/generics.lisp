(in-package :agent-host)

;; Generics
(defgeneric add-agent (host agent)
  (:documentation "Add the agent to the host container."))
(defgeneric remove-agent (host agent)
  (:documentation "Remove and terminate the agent in the host."))
(defgeneric run-once (host)
  (:documentation "Run a single iteration of the event loop and return."))
(defgeneric run (host)
  (:documentation "Run the host until there are no more agents registered."))
