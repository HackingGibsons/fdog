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


;; `:link' command handling
;;         child supervision
(defclass link-manager ()
  ((links :initform (make-hash-table :test 'equalp)
          :accessor links
          :documentation "Hash table of string keys => `standard-watch-machine' instances.
Represents all of the things being supervised by this behavior.")

   (pids :initform (make-hash-table :test 'equalp)
         :accessor pids
         :documentation "Hash table of pids => `links' keys."))

  (:documentation "Mixin to the `create-links' behavior.
Gives storage space to the hash table of stable 'thing' keys to `standard-watch-machine' derivatives based on
the type of 'thing' being linked."))

(defgeneric link-key (behavior what info)
  (:documentation "Generate a hash table string key for the thing described by `what' and `info'.
`what' takes the form of something like `:agent' or `:process' and used to find the right key constructor.
`info' is a representation of the thing to watch and enough information to recreate it with a `:make' message.")
  (:method ((b link-manager) what info)
   (error "No link key for ~A" what))

  (:method ((behavior link-manager) (what (eql :agent)) (info list))
    "Generates an agent-uuid hash key, used in `link-init' and `link-event' to insert
and drive the `standard-watch-machine'"
    (let ((uuid (getf info :uuid)))
      (and uuid
           (format nil "agent-~A" uuid))))

  (:method ((behavior link-manager) (what (eql :process)) (info list))
   "Generates a process hash key used in `link-init' and `link-event' to insert and drive the `standard-watch-machine'"
    (let ((hash (hash-process info behavior)))
      (and hash
           (format nil "process-~A" hash)))))

(defmethod hash-process ((info list) (behavior link-manager))
  "Converts process info into a hash to lookup the right state machine.
If a path and args exist, return a hash of the pid/args.
If a pid exists, use the pid to look up the hash in the behavior's `pids' table."
  (let ((path (getf info :path))
        (args (getf info :args))
        (pid (getf info :pid)))
    (cond
      ((and path args)
       (crypto:byte-array-to-hex-string (crypto:digest-sequence :sha256 (format nil "~A ~{~A ~}" path args))))
      (pid
        (gethash pid (pids behavior))))))

(defgeneric link-init (behavior what info)
  (:documentation "Dispatch to the right method to construct an item
 of the type `what' with the reconstruction information `info' and insert it into the watch table.
Parameter meanings are the same as for `link-key'")

  (:method ((b link-manager) w i)
    "Default operation is a no-op" nil))

(defgeneric link-event (behavior what info)
  (:documentation "Dispatch to the right method to potentially drive a `standard-watch-machine' if one is found.
Parameter meanings are the same as `link-key'")
  (:method ((b link-manager) w i)
    "Default operation is a no-op" nil))

(defbehavior create-links (:or ((:on (:command :link :from :head))
                                (:on (:saw :process :from :eye))
                                (:on (:saw :agent :from :eye))
                                (:on (:made :process :from :hand)))
                               :include (link-manager) :do :invoke-with-event) (organ event)
  ;; Message format: `:link' message:
  ;; https://github.com/vitrue/fdog/wiki/Internal-Messages
  (handler-case
  (cond
    ((getf event :saw)
     (create-links-saw behavior organ event))

    ((getf event :link)
     (create-links-link behavior organ event))

    ((getf event :made)
     (create-links-made behavior organ event)))
    (t (cond) (log-for (error) cond))))

(defmethod create-links-saw ((behavior create-links) (organ standard-organ) event)
  "A handler for `:saw' type of events of the `create-links' behavior.
Fires messages into `link-event' to attempt to drive a `standard-watch-machine' after deconstructing the message to determine the type
and info of the object."
  (let* ((saw-what (getf event :saw))
         (saw-info (and saw-what
                        (getf event saw-what))))
    (link-event behavior saw-what saw-info)))

(defmethod create-links-link ((behavior create-links) (organ standard-organ) event)
  "A handler for the initial `:link' event for the `create-links' behavior.
Fires the `link-init' method after destructurinng the `event' to determine the type and info
of the object to be linked."
  (let* ((link-what (getf event :link))
         (link-info (and link-what
                         (getf event link-what))))
    (link-init behavior link-what link-info)))

(defmethod create-links-made ((behavior create-links) (organ standard-organ) event)
  "A handler for `:made' event of the `create-links' behavior.
Fires messages into the `link-event' method after destructuring the event to determine the type and info of the object."
  (let* ((made-info (getf event :made))
         (made-what (and made-info
                         (getf event :made))))
    (link-event behavior made-what event)))

(defmethod link-init ((behavior link-manager) (what (eql :agent)) info)
  "Specialization of a watch machine construction for an `:agent' thing type."
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (declare (ignorable value))
      (unless foundp
        ;; Store a state under the generated key
        (setf (gethash key (links behavior))
              (make-instance 'agent-watch-machine :behavior behavior
                             :thing-info info))

        ;; Watch an agent
        ;; TODO: This should move to the event machine
        (send-message (behavior-organ behavior) :command `(:command :watch
                                                :watch (:agent :uuid :uuid ,(getf info :uuid))))))))

(defmethod link-event ((behavior link-manager) (what (eql :agent)) info)
  "Specialization of a watch machine event driving for an `:agent' thing type."
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (when (and foundp value)
        (funcall value info)))))

;; Agent-specific watch machine
(defclass agent-watch-machine (standard-watch-machine)
  ()
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A specialization of the `standard-watch-machine' that
knows how to create agents."))

;; Agent specific :initial state for agent watching,
;; Spoilers: It makes the agent the rest of the machine watches
(defstate agent-watch-machine :initial (info)
  "An `:initial' event for `agent-watch-machine', asks for the construction
of an agent and transitions to the `:made' state"
  (log-for (trace watch-machine) "Running :inital event of ~A" machine)
  (setf (getf (timestamps machine) :made) nil)
  (send-message (behavior-organ (behavior machine)) :command
                `(:command :make
                           :make :agent
                           :transaction-id ,(format nil "~A" (uuid:make-v4-uuid))
                           :agent ,(thing-info machine)))
  :made)

;; Process specific watch machine
(defclass process-watch-machine (standard-watch-machine)
  ()
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A specialization of the `standard-watch-machine' that knows how to create processes"))

(defstate process-watch-machine :initial (info)
  "An 'initial' agent for 'process-watch-machine', asks for the construction of a process and transitions to the `:made' state"
  (log-for (trace watch-machine) "Running :initial event of ~A" machine)
  (setf (getf (timestamps machine) :made) nil)
  (send-message (behavior-organ (behavior machine)) :command
                `(:command :make
                  :make :process
                  :process ,(concatenate 'list (thing-info machine) `(:transaction-id ,(format nil "~A" (uuid:make-v4-uuid))))))
  :made)

(defstate process-watch-machine :made (info)
  (let ((pid (getf info :pid)))
    (send-message (behavior-organ (behavior machine)) :command `(:command :watch
                                                                 :watch (:process :pid :pid ,pid)))
    (setf (gethash pid (pids (behavior machine))) (hash-process info (behavior machine)))
    (call-next-method)))

(defstate process-watch-machine :watch (info)
  (unless (getf info :alive)
    (log-for (warn watch-machine) "process has died")
    :died))

(defstate process-watch-machine :died (info)
  (remhash (getf info :pid) (pids (behavior machine)))
  (call-next-method))

(defmethod link-init ((behavior link-manager) (what (eql :process)) info)
  "Specialization of a watch machine construction for an `:process' thing type."
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (declare (ignorable value))
      (unless foundp
        ;; Store a state under the generated key
        (setf (gethash key (links behavior))
              (make-instance 'process-watch-machine :behavior behavior
                             :thing-info info))))))

(defmethod link-event ((behavior link-manager) (what (eql :process)) info)
  "Specialization of a watch machine event driving for an `:process' thing type."
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (when (and foundp value)
        (funcall value info)))))
