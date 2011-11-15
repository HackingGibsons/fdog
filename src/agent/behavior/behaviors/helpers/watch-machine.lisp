(in-package :agent)

(defcategory watch-machine)

(defclass standard-watch-machine (standard-state-machine)
  ((behavior :initform nil :initarg :behavior
             :accessor behavior
             :documentation "The behavior that owns this watch-machine")
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

On `initialize-instance' a `:boot' event is fired into the machine into the current event. If your machine
does not require bootstrapping, ensure this event does not fault your state transition.

NOTE: The default implementation does not provide an `:initial' state, and should not be instantiated directly.
Provide an `:initial' state in your subclass of the watch machine that knows how to construct your objects,
and look over the `:made' and `:watch' states to consider overriding them.

SUBCLASS NOTE: Make sure to include ```(:metaclass c2mop:funcallable-standard-class)``` in your
subclass definition, or else the funcallable instance will not function correctly."))

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
  (log-for (watch-machine warn) "In :make-fail of ~A" machine)
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
  (log-for (warn watch-machine) "[WARN] Dead, restarting ~A => ~A" machine info)
  :initial)
