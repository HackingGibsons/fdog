(in-package :api-agent)

;; Knobs
(defvar *timeout-interval* 10 "Timeout length, in seconds")

;; Class
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
    :initform (* *timeout-interval* internal-time-units-per-second) ;; in seconds
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

(defgeneric register-callback (agent callback))

(defmethod register-callback ((agent api-agent) (callback callback))
  "Adds a `callback' instance to the agent's `callbacks' list"
  (appendf (callbacks agent) callback))

(defmethod heard-message :after ((agent api-agent) (head agent-head) (from (eql :agent)) type &rest event)
  "After hearing an event message, test it against the predicates for the callbacks registered
to the agent. If any match, call the callback and unregister it."
  (dolist (callback (callbacks agent))
    (when (funcall (predicate callback) event)
      (funcall (callback callback))
      (remove callback callbacks))))

(defbehavior increment-callback-timeouts (:on (:interval (:from :heart :nth 1)) :do :invoke-with-event) (organ event)
  ;;; Every heart beat, check the registered callbacks for timeout. If
  ;;; they have timed out, fire the `timeout-callback' and unregister.
  (let* ((time (get-internal-real-time))
         (agent (organ-agent organ))
         (callbacks (callbacks agent)))
    (dolist (callback callbacks)
      (when (>= (timeout callback) (- time (start-time callback)))
        (funcall (timeout-callback callback))
        (remove callback callbacks)))))
