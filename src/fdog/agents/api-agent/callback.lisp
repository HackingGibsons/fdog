(in-package :api-agent)

;; Knobs
(defvar *timeout-interval* 10 "Timeout length, in seconds")

(defcategory callback)

;; Class
(defclass callback ()
  ((predicate
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
  (appendf (callbacks agent) (list callback)))

(defmethod heard-message :after ((agent api-agent) (head agent-head) from type &rest event)
  "After hearing an event message, test it against the predicates for the callbacks registered
to the agent. If any match, call the callback and unregister it."
  (log-for (trace callback) "Heard a message, checking callback.")
  (let ((callbacks (callbacks agent)))
    (dolist (callback callbacks)
      (log-for (trace callback) "Message ~S match ~A" event (funcall (predicate callback) from type event))
      (when (funcall (predicate callback) from type event)
        (funcall (callback callback))
        (remove callback callbacks)))))

(defbehavior increment-callback-timeouts (:interval (:from :heart :nth 1) :do :invoke) (organ)
  ;;; Every heart beat, check the registered callbacks for timeout. If
  ;;; they have timed out, fire the `timeout-callback' and unregister.
  (log-for (trace callback) "Checking timeouts")
  (let* ((time (get-internal-real-time))
         (agent (organ-agent organ))
         (callbacks (callbacks agent)))
    (dolist (callback callbacks)
      (when (>= (timeout callback) (- time (start-time callback)))
        (log-for (trace callback) "Timeout reached")
        (funcall (timeout-callback callback))
        (remove callback callbacks)))))

(defmethod agent-special-event :after ((agent api-agent) (event-head (eql :boot)) event)
  (make-increment-callback-timeouts (find-organ agent :head)))
