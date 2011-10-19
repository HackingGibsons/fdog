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

;; Child agent mixin
(defclass standard-child-mixin ()
  ((parent-uuid :initarg :parent-uuid
                :reader parent-uuid)
   (parent-mouth :initarg :parent-mouth
                 :reader parent-mouth))
  (:documentation "An agent that has a parent which it listens to, and dies without."))

(defmethod agent-special-event :after ((agent standard-child-mixin) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let ((head (find-organ agent :head)))
    (send-message head `(,(organ-tag head) :command
                          :command :listen
                          :uuid ,(organ-uuid head)
                          :listen ,(parent-mouth agent)))))
;; An leaf agent base
(defclass standard-leaf-agent (standard-agent standard-child-mixin)
  ())
