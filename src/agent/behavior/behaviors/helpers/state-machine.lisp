(in-package :agent)

(defcategory state-machine)

(defclass standard-state-machine (c2mop:funcallable-standard-object)
  ((behavior :initform nil :initarg :behavior
             :accessor behavior
             :documentation "The behavior that owns this state-machine")
   (state :initform :initial :initarg :state
          :accessor state
          :documentation "The current state of the state-machine.")
   (last-event :initform (get-internal-real-time)
               :accessor last-event
               :documentation "The timestamp of the last event.")

   (timestamps :initform nil
               :accessor timestamps
               :documentation "A plist of timestamps that some events might need to use.
Like the created-at date of a thing.")
   (fail-after :initform (* internal-time-units-per-second 15)
               :reader fail-after
               :documentation "The interval of time in internal time units to wait for the construction of a thing."))

  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "(funcall this-instance event-from-bus)
Every iteration of the event machine the `last-event' slot is updated with `get-internal-real-time' before
the funcallable instance application.

On `initialize-instance' a `:boot' event is fired into the machine into the current event. If your machine
does not require bootstrapping, ensure this event does not fault your state transition.

SUBCLASS NOTE: Make sure to include ```(:metaclass c2mop:funcallable-standard-class)``` in your
subclass definition, or else the funcallable instance will not function correctly."))

(defmethod initialize-instance :before ((machine standard-state-machine) &key)
  "Bind a (funcallable machine event) driver to the event machine instance.
See `defstate' for the reasoning and function. This method is closure plumbing."
  (c2mop:set-funcallable-instance-function
   machine
   #'(lambda (event)
       (log-for (state-machine trace) "~A event ~A" machine event)
       (multiple-value-bind (next-state recur-p)
           (handler-case (standard-state-machine-event machine (state machine) event)
             (simple-error () (error "State ~A of ~A is not defined." (state machine) machine)))

         (log-for (state-machine trace) "Next state: ~A Recur?: ~A" next-state recur-p)
         (setf (last-event machine) (get-internal-real-time)
               (state machine) (or next-state (state machine)))

         (if recur-p
             (funcall machine event)
             (values machine (state machine)))))))

(defmethod initialize-instance :after ((machine standard-state-machine) &key)
  "Fire a boot event in order to get the machine into the :initial state."
  (funcall machine '(:event :boot)))

(defmacro defstate (machine-type state-name (event-sym) &body body)
  "Helper macro to define states for the machine of type `machine-type'.

The generated state methods will be specialized on `machine-type' and `state-name', and
subclasses of `standard-state-machine' should use this property to extend the state machine.

`state-name' is the identifier for this state, and names it. Event invocations will
use this name to determine which state the machine is in, and error out if one cannot be found.
The event will be bound to the symbol named `event-sym' declared as in a one-argument lambda list.
Each invocation of this state with the even bound to `event-sym' will evaluate `body' forms as
in a method invocation and the resulting value of the evaluation should return the next state
for the machine as a `:keyword', or `nil' to indicate the machine should remain in its current state.
The symbol `machine' will be bound to the currently executing state machine. The current state is
available in `state'

If the state produces two-value return, it is interpreted as (values next-state recur-event)
and if recur-event is non-nil the same event is sent into the machine again after performing
the transition into next-state. This is useful if simply performing a state transition would
result in event starvation."
  `(defmethod standard-state-machine-event
       ((machine ,machine-type) (state (eql ,state-name)) ,event-sym)
     ,@body))

;; Default state
(defstate standard-state-machine :boot (info)
  "This event is an entry point to get the machine into the :initial state without waiting for an event to fire"
  (log-for (trace state-machine) "Booting ~A with :boot" machine)
  :initial)
