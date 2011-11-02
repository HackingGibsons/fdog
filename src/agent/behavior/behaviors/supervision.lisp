(in-package :agent)

(defclass lonely-mixin ()
  ((last-seen :initform (get-internal-real-time)
              :accessor last-seen)
   (lonely-tolerance :initform (* 10 internal-time-units-per-second)
                     :accessor lonely-tolerance)))

;; TODO: Maybe something more like (:need (:agent :uuid :from #'parent-uuid) :do invoke) ?
(defbehavior die-without-parent (:interval (:from :heart :nth 6) :include (lonely-mixin) :do :invoke) (organ)
  (let ((parent (gethash (parent-uuid (organ-agent organ)) (agent-peers organ)))
        (now (get-internal-real-time)))

    (when parent
      (setf (last-seen behavior) (getf parent :time 0)))

    (when (<= (lonely-tolerance behavior)
              (- now (last-seen behavior)))
      (log-for (warn) "~A: My parent has died." organ)
      (suicide (organ-agent organ)))))

(defclass link-manager ()
  ((links :initform (make-hash-table :test 'equalp)
          :accessor links)))

(defgeneric link-key (behavior what info)
  (:documentation "Generate a hash table string key for the thing described by `what' and `info'")
  (:method (b what info) "Default operation is a noop" nil)

  (:method ((behavior link-manager) (what (eql :agent)) (info list))
    "Generates an agent-uuid hash key"
    (let ((uuid (getf info :uuid)))
      (and uuid
           (format nil "agent-~A" uuid)))))

(defgeneric link-init (behavior what info)
  (:method (b w i) nil))

(defgeneric link-event (behavior what info)
  (:method (b w i) nil))

(defbehavior create-links (:or ((:on (:command :link :from :head))
                                (:on (:saw :process :from :eye))
                                (:on (:saw :agent :from :eye)))
                               :include (link-manager) :do :invoke-with-event) (organ event)
  ;; (:command :link
  ;;           :link :agent
  ;;           :agent (:uuid ,uuid :class leaf-test-agent :package :afdog-tests)
  ;;   --or --
  ;;           :link :process
  ;;           :process (:pid pid :make (:cmd "string" :args ("list" "of" "strings") :pwd ""))
  ;;           .. or something

  ;; TODO: Not this, write a dispawtching :do for compound events
  (cond
    ((getf event :saw)
     (create-links-saw behavior organ event))

    ((getf event :link)
     (create-links-link behavior organ event))))

(defmethod create-links-saw ((behavior create-links) (organ standard-organ) event)
  (let* ((saw-what (getf event :saw))
         (saw-info (and saw-what
                        (getf event saw-what))))
    (link-event behavior saw-what saw-info)))

(defmethod create-links-link ((behavior create-links) (organ standard-organ) event)
  (let* ((link-what (getf event :link))
         (link-info (and link-what
                    (getf event link-what))))
    (link-init behavior link-what link-info)))

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
  (format t "Looping main event!~%")
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
    (format t "[WARN] ~A is still failing.~%" machine)))

(defstate standard-watch-machine :watch (info)
  (format t "Watching: ~A~%" machine)
  (unless info
    (format t "[WARN] Going to die ~A => ~A~%" machine info)
    :died))

(defstate standard-watch-machine :died (info)
  (format t "[WARN] Dead, restarting ~A => ~A~%" machine info)
  :initial)


;; Agent-specific watch machine
(defclass agent-watch-machine (standard-watch-machine)
  ()
  (:metaclass c2mop:funcallable-standard-class))

;; Agent specific :initial state for agent watching,
;; Spoilers: It makes the agent the rest of the machine watches
(defstate agent-watch-machine :initial (info)
  (format t "Running :inital event of ~A~%" machine)
  (setf (getf (timestamps machine) :made) nil)
  (send-message (behavior-organ (behavior machine)) :command
                `(:command :make
                           :make :agent
                           :transaction-id ,(format nil "~A" (uuid:make-v4-uuid))
                           :agent ,(thing-info machine)))
  :made)




;; Agent init and event
(defmethod link-init ((behavior link-manager) (what (eql :agent)) info)
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (declare (ignorable value))
      (unless foundp
        ;; Store a state under the generated key
        (setf (gethash key (links behavior))
              (make-instance 'agent-watch-machine :behavior behavior
                             :thing-info info))
        ;;    `(:state :initial :time ,(get-internal-real-time) :what ,what :how ,info))

        ;; Watch an agent
        ;; TODO: This should move to the event machine
        (send-message (behavior-organ behavior) :command `(:command :watch
                                                :watch (:agent :uuid :uuid ,(getf info :uuid))))))))

(defmethod link-event ((behavior link-manager) (what (eql :agent)) info)
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (when (and foundp value)
        (funcall value info)))))
