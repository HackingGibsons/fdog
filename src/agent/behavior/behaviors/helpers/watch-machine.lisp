(in-package :agent)

(defcategory watch-machine)

(defclass standard-watch-machine (c2mop:funcallable-standard-object)
  ((behavior :initform nil :initarg :behavior
             :accessor behavior
             :documentation "The behavior that owns this watch-machine")
   (state :initform :initial :initarg :state
          :accessor state
          :documentation "The current state of the watch-machine.")
   (last-event :initform (get-internal-real-time)
               :accessor last-event
               :documentation "The timestamp of the last event.")

   (timestamps :initform nil
               :accessor timestamps
               :documentation "A plist of timestamps that some events might need to use.
Like the created-at date of a thing.")
   (fail-after :initform (* internal-time-units-per-second 15)
               :reader fail-after
               :documentation "The interval of time in internal time units to wait for the construction of a thing.")
   (thing-info :initform nil :initarg :thing-info
               :accessor thing-info
               :documentation "The information required to re-construct the object initially and repeatedly in case of failure."))

  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "The `standard-watch-machine' is a class for storing state about a watchable 'thing'.

It is used by `create-links' behavior to drive the supervision machinery. Every item in the watched items
table is an instance of a subclass of this class. It is funcalled as follows every time a :saw, and in the future
:made event triggers the behavior, and the event is passed in:
  (funcall this-instance event-from-bus)
Every iteration of the event machine the `last-event' slot is updated with `get-internal-real-time' before
the funcallable instance application.

NOTE: The default implementation does not provide an `:initial' state, and should not be instantiated directly.
Provide an `:initial' state in your subclass of the watch machine that knows how to construct your objects,
and look over the `:made' and `:watch' states to consider overriding them.

SUBCLASS NOTE: Make sure to include ```(:metaclass c2mop:funcallable-standard-class)``` in your
subclass definition, or else the funcallable instance will not function correctly."))

(defmethod initialize-instance :before ((machine standard-watch-machine) &key)
  "Bind a (funcallable machine event) driver to the event machine instance.
See `defstate' for the reasoning and function. This method is closure plumbing."
  (c2mop:set-funcallable-instance-function
   machine
   #'(lambda (event)
       (let ((next-event (handler-case (watch-machine-event machine (state machine) event)
                           (simple-error () (error "State ~A of ~A is not defined." (state machine) machine)))))
         (setf (last-event machine) (get-internal-real-time)
               (state machine) (or next-event (state machine)))
       (values machine (state machine))))))

(defmethod initialize-instance :after ((machine standard-watch-machine) &key)
  "Fire a boot event in order to get the machine into the :initial state."
  (funcall machine '(:event :boot)))

(defmacro defstate (machine-type state-name (event-sym) &body body)
  "Helper macro to define states for the machine of type `machine-type'.

The generated state methods will be specialized on `machine-type' and `state-name', and
subclasses of `standard-watch-machine' should use this property to extend the state machine.

`state-name' is the identifier for this state, and names it. Event invocations will
use this name to determine which state the machine is in, and error out if one cannot be found.
The event will be bound to the symbol named `event-sym' declared as in a one-argument lambda list.
Each invocation of this state with the even bound to `event-sym' will evaluate `body' forms as
in a method invocation and the resulting value of the evaluation should return the next state
for the machine as a `:keyword', or `nil' to indicate the machine should remain in its current state.
The symbol `machine' will be bound to the currently executing state machine. The current state is
available in `state'"
  `(defmethod watch-machine-event ((machine ,machine-type) (state (eql ,state-name)) ,event-sym)
     ,@body))

;; Default states of thingwatching
(defstate standard-watch-machine :boot (info)
  "This event is an entry point to get the machine into the :initial state without having to wait on a watch event that might not exist yet."
  :initial)

(defstate standard-watch-machine :made (info)
  "This event is responsible for making sure creation of a 'thing' occurs within a defined
interval, and storing the time that occured in the `timestamps' plist of the machine."
  (log-for (trace watch-machine) "Looping main event!")
  (unless (getf (timestamps machine) :made)
    (setf (getf (timestamps machine) :made) (last-event machine)))

  (cond (info
         :watch)
        ((>= (- (last-event machine) (getf (timestamps machine) :made))
             (fail-after machine))
         :make-fail)))

(defstate standard-watch-machine :make-fail (info)
  "A transition state into failure"
  :failed)

(defstate standard-watch-machine :failed (info)
  "This state executes forever while whining that the current machine
is in a failure state."
  (prog1 nil
    (log-for (watch-machine warn) "[WARN] ~A is still failing." machine)))

(defstate standard-watch-machine :watch (info)
  "The stable state of the machine after creation. `info' should contain the `:saw' message
for the given object, which should be checked for validity"
  (log-for (trace watch-machine) "Watching: ~A => ~A" machine info)
  (unless info
    (log-for (warn watch-machine) "Going to die.")
    :died))

(defstate standard-watch-machine :died (info)
  "State of a dead item. Re-transitions to the `:initial' state of the current machine
to attempt to recreate the item."
  (log-for (watch-machine) "[WARN] Dead, restarting ~A => ~A" machine info)
  :initial)


