(in-package :agent)

(defcategory watch-machine)

(defclass standard-watch-machine (c2mop:funcallable-standard-object)
  ((behavior :initform nil :initarg :behavior
             :accessor behavior)
   (state :initform :initial
          :accessor state)
   (last-event :initform (get-internal-real-time)
               :accessor last-event)

   (timestamps :initform nil
               :accessor timestamps
               :documentation "A plist of timestamps that some events might need to use.
Like the created-at date of a thing.")
   (fail-after :initform (* internal-time-units-per-second 15)
               :reader fail-after)
   (thing-info :initform nil :initarg :thing-info
               :accessor thing-info))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :before ((machine standard-watch-machine) &key)
  "Bind a (funcallable machine event)  driver to the event machine instance."
  (c2mop:set-funcallable-instance-function
   machine
   #'(lambda (event)
       (let ((next-event (handler-case (watch-machine-event machine (state machine) event)
                           (simple-error () (error "State ~A of ~A is not defined." (state machine) machine)))))
         (setf (last-event machine) (get-internal-real-time)
               (state machine) (or next-event (state machine)))
       (values machine (state machine))))))

(defmacro defstate (machine-type state-name (event-sym) &body body)
  "Helper macro to define states for the machine of type named `machine-type'"
  `(defmethod watch-machine-event ((machine ,machine-type) (state (eql ,state-name)) ,event-sym)
     ,@body))

;; Default states of thingwatching
(defstate standard-watch-machine :made (info)
  (log-for (watch-machine) "Looping main event!")
  (unless (getf (timestamps machine) :made)
    (setf (getf (timestamps machine) :made) (last-event machine)))

  (cond (info :watch)
        ((>= (- (last-event machine) (getf (timestamps machine) :made))
             (fail-after machine))
         :make-fail)))

(defstate standard-watch-machine :make-fail (info)
  :failed)

(defstate standard-watch-machine :failed (info)
  (prog1 nil
    (log-for (watch-machine) "[WARN] ~A is still failing." machine)))

(defstate standard-watch-machine :watch (info)
  (log-for (watch-machine) "Watching: ~A => ~A" machine info)
  (unless info
    (log-for (watch-machine) "[WARN] Going to die.")
    :died))

(defstate standard-watch-machine :died (info)
  (log-for (watch-machine) "[WARN] Dead, restarting ~A => ~A" machine info)
  :initial)


