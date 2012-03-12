(in-package :agent)

;; Globals
(defparameter *context-threads* 1
  "Number of threads an agent context uses.")
(defparameter *event-starvation-timeout* 10
  "Number of seconds that an agent can live without events.")

;; Classes
(defclass standard-agent ()
  ((uuid :initarg :uuid
         :reader agent-uuid
         :initform (print-object (uuid:make-v4-uuid) nil))

   (context :initarg :context
            :accessor agent-context
            :initform nil)
   ;; Subscription addr/sock pair
   (event-addr :initarg :event-addr
               :reader agent-event-addr)
   (event-sock :accessor agent-event-sock)
   ;; Publish addr/sock pair
   (message-addr :initarg :message-addr
                 :reader agent-message-addr)
   (message-sock :accessor agent-message-sock)

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
               :accessor agent-tick-delta)

   (universal-start-time :initform (get-universal-time)
                         :reader universal-start-time)
   (start-time :initform (get-internal-real-time)
               :reader start-time))

  (:documentation "A standard agent shell. Capable of communication, but completely dead inside."))

;; Rooted agent mixin
(defclass rooted-agent-mixin ()
  ((root :initarg :root :initform *root*
         :accessor agent-root)))

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
    (make-die-without-parent head)

    (send-message head :command `(:command :listen
                                  :uuid ,(organ-uuid head)
                                  :listen ,(parent-mouth agent)))))

(defmethod agent-info :around ((agent standard-child-mixin))
  (append (call-next-method)
          `(:parent ,(parent-uuid agent))))

;; Parent agent tools
(defclass standard-supervisor-mixin ()
  ()
  (:documentation "A mixin to enable supervision by this agent."))

(defmethod agent-special-event :after ((agent standard-supervisor-mixin) (event-head (eql :boot)) event)
  "Boot event for a child agent."
  (let ((head (find-organ agent :head)))
    (make-create-links head)))

;; Combinations of agents
(defclass standard-leaf-agent (standard-agent standard-child-mixin)
  ())

(defclass standard-hypervisor-agent (standard-agent standard-supervisor-mixin)
  ())

(defclass standard-manager-agent (standard-agent standard-child-mixin standard-supervisor-mixin)
  ())
