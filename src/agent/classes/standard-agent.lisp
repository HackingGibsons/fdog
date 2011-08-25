(in-package :agent)

;; Globals
(defparameter *context-threads* 1
  "Number of threads an agent context uses.")
(defparameter *event-starvation-timeout* 3
  "Number of seconds that an agent can live without events.")

;; Classes
(defclass standard-agent ()
  ((uuid :initarg :uuid
         :reader agent-uuid
         :initform (print-object (uuid:make-v4-uuid) nil))

   (context :initarg :context
            :reader agent-context
            :initform nil)
   ;; Subscription addr/sock pair
   (event-addr :initarg :event-addr
               :reader agent-event-addr)
   (event-sock :reader agent-event-sock)
   ;; Publish addr/sock pair
   (message-addr :initarg :message-addr
                 :reader agent-message-addr)
   (message-sock :reader agent-message-sock)

   ;; Squigly spooges
   (organs :initform nil
           :accessor agent-organs)

   ;; Cron
   (cron :initform nil
         :accessor agent-cron
         :documentation "A list of integers in the form of `(get-internal-real-time)' at which ticks should fire.")

   ;; Event, tick and timeout bookeeping
   (event-count :initform 0
                :accessor agent-event-count)
   (last-event :initform (get-internal-real-time)
               :accessor agent-last-event)
   (last-tick :initform 0
              :accessor agent-last-tick)
   (tick-delta :initform 0
               :accessor agent-tick-delta))

  (:documentation "A standard agent shell. Capable of communication, but completely dead inside."))

