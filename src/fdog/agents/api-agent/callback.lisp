(in-package :api-agent)

(defclass callback ()
  ((transaction-id
    :accessor transaction-id
    :initform (error "Transaction id is required")
    :documentation "A uuid for the callback matching the transaction id of the associated request.")
   (predicate
    :accessor predicate
    :initarg :predicate
    :initform t
    :documentation "An optional predicate to test against incoming messages for when to fire the callback.")
   (callback
    :accessor callback
    :initarg :callback
    :initform (error "Callback is required")
    :documentation "The function to callback when the predicate matches.")
   (start-time
    :accessor start-time
    :initform (get-internal-real-time)
    :documentation "The time the callback was registered.")
   (timeout
    :accessor timeout
    :initform (* 10 internal-time-units-per-second) ;; in seconds
    :documentation "Number of seconds until the callback times out.")
   (timeout-callback
    :accessor timeout-callback
    :initarg :timeout-callback
    :initform (error "Timeout callback is required")
    :documentation "The callback to fire if the callback times out."))
  (:documentation "A callback for API requests to hook into when waiting on a response from another agent.

Every iteration of the event loop, all registered callbacks will be tested against their
predicates. If the predicate passes, the callback is fired and the callback is unregistered.

Every time the agent hears a beat, if an agent's (current-time - start-time)
exceeds the timeout threshold, the timeout callback is fired and the callback is unregistered"))
