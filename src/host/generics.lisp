(in-package :agent-host)

;; Generics
(defgeneric add-agent (host agent)
  (:documentation "Add the agent to the host container."))
(defgeneric register-agent (host agent)
  (:documentation "Register the agent in the container and add it to
the event loop."))
(defgeneric has-agent-p (host agent)
  (:documentation "Predicate to determine if a given agent is running within this loop."))
(defgeneric remove-agent (host agent)
  (:documentation "Schedule to remove and terminate the agent in the host."))
(defgeneric evict-agent (host agent)
  (:documentation "Remove and disconnect the agent from the event loop."))
(defgeneric run-once (host)
  (:documentation "Run a single iteration of the event loop and return."))
(defgeneric run (host)
  (:documentation "Run the host until there are no more agents registered."))
