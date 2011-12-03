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
         :documentation "Hash table of pids => `links' keys.")

   (transactions :initform (make-hash-table :test 'equalp)
                 :accessor transactions
                 :documentation "A table mapping transactions to `links' keys for process creation."))

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
   "Generates a process hash key used in `link-init' and `link-event' to insert and drive the `standard-watch-machine'
Converts process info into a hash to lookup the right state machine.
If a path and args exist, return a hash of the pid/args.
If a pid exists, use the pid to look up the hash in the behavior's `pids' table."
   (let ((path (getf info :path))
         (args (getf info :args))
         (pid (getf info :pid)))
     (log-for (trace watch-machine) "Hasing process: ~A" info)
     (cond
       (pid
        (log-for (trace watch-machine) "Looking up by pid: ~A" pid)
        (log-for (trace watch-machine) "Found: ~A" (gethash pid (pids behavior)))
        (gethash pid (pids behavior)))

       (path
        (log-for (trace watch-machine) "Using path(~A) and args(~A) to make key." path args)
        (format nil "process-~A"
                (crypto:byte-array-to-hex-string
                 (crypto:digest-sequence :sha256 (babel:string-to-octets (format nil "~A ~{~A ~}" path args))))))))))

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
                                (:on (:made :process :from :hand))
                                (:on (:command :unlink :from :head)))
                               :include (link-manager) :do :invoke-with-event) (organ event)
  ;; Message format: `:link' message:
  ;; https://github.com/vitrue/fdog/wiki/Internal-Messages
  (log-for (trace watch-machine) "`create-links' Event: ~A" event)
  (cond
    ((getf event :saw)
     (create-links-saw behavior organ event))

    ((getf event :link)
     (create-links-link behavior organ event))

    ((getf event :made)
     (create-links-made behavior organ event))

    ;; Unlink is part of this behavior to have access to the same links hashtable
    ;; (they're bound to behaviors)
    ((getf event :unlink)
     (create-links-unlink behavior organ event))))

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
  (let* ((made-what (getf event :made))
         (made-info (and made-what
                         (getf event made-what))))
    (log-for (trace watch-machine) "`create-links-made': What: ~A Info: ~A" made-what made-info)
    (link-event behavior made-what made-info)))

(defmethod create-links-unlink ((behavior create-links) (organ standard-organ) event)
             (log-for (trace) "destroy behavior entered")
  (let* ((unlink-what (getf event :unlink))
         (unlink-info (and unlink-what
                           (getf event unlink-what))))
    (log-for (trace) "what: ~A event: ~A" unlink-what event)
    (unlink behavior unlink-what event)))

(defmethod create-links-unlink ((behavior create-links) (organ standard-organ) event)
             (log-for (trace) "destroy behavior entered")
  (let* ((unlink-what (getf event :unlink))
         (unlink-info (and unlink-what
                           (getf event unlink-what))))
    (log-for (trace) "what: ~A event: ~A" unlink-what event)
    (unlink behavior unlink-what event)))

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

(defmethod unlink ((behavior link-manager) (what (eql :agent)) info)
  (let ((key (link-key behavior what info))
        (uuid (getf info :uuid)))
    ;; Look up the agent in the links table
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (when foundp
        ;; when found, remove machine from list
        (remhash key (links behavior))
        ;; Stop watching the thing
        (send-message (behavior-organ behavior) :command `(:command :stop-watching
                                                                    :stop-watching (:agent :uuid :uuid ,uuid)))
        ;; Send a callback
        (send-message (behavior-organ behavior) :unlinked `(:unlinked :agent :uuid ,uuid))))))

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
  ((key :initform nil :initarg :key :accessor key))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A specialization of the `standard-watch-machine' that knows how to create processes"))

(defstate process-watch-machine :initial (info)
  "An 'initial' agent for 'process-watch-machine', asks for the construction of a process and transitions to the `:made' state"
  (log-for (trace watch-machine) "Running :initial event of ~A" machine)
  (log-for (trace watch-machine) "Thing info: ~A" (thing-info machine))
  (setf (getf (timestamps machine) :made) nil)
  (let ((transaction (format nil "~A" (uuid:make-v4-uuid))))
    (setf (gethash transaction (transactions (behavior machine))) (key machine))
    (log-for (trace watch-machine) "Trans storage: ~A" (transactions (behavior machine)))

    (send-message (behavior-organ (behavior machine)) :command
                  `(:command :make
                             :make :process
                             :process ,(concatenate 'list
                                                    (thing-info machine)
                                                    `(:transaction-id ,transaction)))))
  :made)

(defstate process-watch-machine :made (info)
  (log-for (trace watch-machine) ":made state of ~A => ~A" machine info)
  (let ((pid (cond ((and (eql (car info) :event) (eql (getf info :event) :boot))
                    (getf (thing-info machine) :pid))
                   (t
                    (getf info :pid)))))
    (if (not pid)
        (prog1 nil
          (log-for (watch-machine trace)  ":made state wants to run with no pid. Event: ~A" info))

        (progn
          (send-message (behavior-organ (behavior machine)) :command `(:command :watch
                                                                                :watch (:process :pid :pid ,pid)))
          (setf (gethash pid (pids (behavior machine))) (key machine))
          (call-next-method)))))

(defstate process-watch-machine :watch (info)
  (log-for (watch-machine trace) "In :watch state of ~A" machine)
  (unless (getf info :alive)
    (log-for (warn watch-machine) "process has died")
    :died))

(defstate process-watch-machine :died (info)
  (log-for (watch-machine trace) "In :died state of ~A" machine)
  (let ((pid (getf info :pid)))
    (remhash pid (pids (behavior machine)))
    (log-for (watch-machine trace) "Stopping the old watch of ~A" pid)
    (send-message (behavior-organ (behavior machine)) :command `(:command :stop-watching
                                                                 :stop-watching (:process :pid :pid ,pid))))
  (values (call-next-method)
          :fire-again))

(defmethod link-init ((behavior link-manager) (what (eql :process)) info)
  "Specialization of a watch machine construction for an `:process' thing type."
  (let ((key (link-key behavior what info)))
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (declare (ignorable value))
      (if foundp
          (progn
            (log-for (trace watch-machine) "Attempting to link existing item: ~A" key)
            (send-message (behavior-organ behavior) :already-linked
                          `(:already-linked ,what
                            ,what ,info)))
          (progn
            (log-for (trace watch-machine) "Making machine: ~A" info)
            ;; Store a state under the generated key
            (setf (gethash key (links behavior))
                  (make-instance 'process-watch-machine :behavior behavior :key key
                                 :state (if (getf info :pid) :made :initial)
                                 :thing-info info)))))))

(defmethod link-event ((behavior link-manager) (what (eql :process)) info)
  "Specialization of a watch machine event driving for an `:process' thing type."
  (let* ((key (link-key behavior what info))
         (transaction (getf info :transaction-id))
         (trans-key (gethash transaction (transactions behavior))))

    (log-for (watch-machine trace) "Looking for machine with key: ~A" (or key trans-key))
    (multiple-value-bind (value foundp) (gethash (or key trans-key) (links behavior))
      (log-for (watch-machine trace) "Found: ~A Value: ~A" foundp value)

      (when trans-key
        (log-for (trace watch-machine) "Removing transaction key: ~A=>~A" transaction trans-key)
        (remhash transaction (transactions behavior)))

      (when (and foundp value)
        (log-for (watch-machine trace) "Sending event ~A at ~A (~A)" info value (state value))
        (funcall value info)))))

(defmethod unlink ((behavior link-manager) (what (eql :process)) info)
  (let ((key (link-key behavior what info))
        (pid (getf info :pid)))
    ;; Look up the process in the links table
    (multiple-value-bind (value foundp) (gethash key (links behavior))
      (when foundp
        ;; when found, remove machine from list
        (remhash key (links behavior))
        ;; Stop watching the thing
        (send-message (behavior-organ behavior) :command `(:command :stop-watching
                                                                    :stop-watching (:process :pid :pid ,pid)))
        ;; Send a callback
        (send-message (behavior-organ behavior) :unlinked `(:unlinked :process :pid ,pid))))))
