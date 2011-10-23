(in-package :agent)

(defbehavior announce-self (:interval (:from :heart :nth 3) :do :invoke) (organ)
  (log-for (trace) "Running ~A behavior lambda for ~A" behavior organ)
  (send-message organ :command `(:command :speak
                                 :say (:agent :info :info ,(agent-info (organ-agent organ)))))
  (log-for (trace) "Message sent."))

(defbehavior speak-when-told (:on (:command :speak :from :head) :do :invoke-with-event) (organ event)
  (let ((message (getf event :say)))
    (send-message organ :raw message :sock (speak-sock organ))
    (log-for (trace) "~A => ~A has spoken: ~A" organ (speak-addr organ) message)))

(defgeneric heard-message (organ from type &rest event)
  (:documentation "A dispatch method for heard messages.")
  (:method ((organ standard-organ) from type &rest event)
    (declare (ignore event))
    (log-for (trace) "~A: Message ~A/~A unhandled." organ from type)))

(defbehavior have-hearing (:on (:heard :message :from :ear) :do :invoke-with-event) (organ event)
  (let ((message (getf event :message)))
    (apply #'heard-message `(,organ ,@message))))

(defbehavior talk-where-told (:on (:command :speak-to :from :head) :do :invoke-with-event) (organ event)
  (let ((addr (getf event :speak-to)))
    (when (and addr (speak-sock organ)
               (not (gethash addr (speaking-to organ))))

      (setf (gethash addr (speaking-to organ))
            (zmq:connect (speak-sock organ) addr)))))

(defbehavior listen-where-told (:on (:command :listen :from :head) :do :invoke-with-event) (organ event)
  (let ((addr (getf event :listen)))
    (when (and addr (listen-sock organ)
               (not (gethash addr (listening-to organ))))
      (setf (gethash addr (listening-to organ))
            (zmq:connect (listen-sock organ) addr)))))

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


(defclass supervisor-mixin ()
  ())

(defmethod agent-spawned ((behavior supervisor-mixin) uuid)
  "Drive the event machine attached to a given peer."
  (format t "Agent spawned as per ~A: ~A~%" behavior uuid)
  :TODO)

(defmethod spawn-agent ((behavior supervisor-mixin) (organ standard-organ) event)
  (format t "Spawn agent: ~A~%" event)
  ;; TODO: Figure out slightly better how to select the spawn class
  (let* ((spec (getf event :spawn))
         (package (find-package (getf spec :package)))

         (class (and package
                     (getf spec :class)))
         (class (and class (find-symbol (symbol-name class) package)))
         (class (and class (handler-case
                               (prog1 (find-class class)
                                 (c2mop:finalize-inheritance (find-class class)))
                             (simple-error () nil))))
         (class (and class
                     (find (find-class 'standard-agent)
                           (c2mop:class-precedence-list class))
                     class)))

    ;; At this point we have assured that `class' is a real class
    ;; and is a subclass of `standard-agent'
    (when (and class package)
      (let* ((uuid (format nil "~A" (uuid:make-v4-uuid)))
             (initargs `(:uuid ,uuid
                         :parent-uuid ,(agent-uuid (organ-agent organ))
                         :parent-mouth ,(mouth-addr (find-organ (organ-agent organ) :mouth))))
             (runner (apply #'make-runner *spawner* :class (class-name class) initargs)))

        (and runner
             (start runner)
             (agent-spawned behavior uuid))))))


(defmethod children-check ((behavior supervisor-mixin) (organ standard-organ))
  (format t "TODO: Check children of ~A~%" behavior)
  :TODO)

(defbehavior spawn-and-watch-children (:or ((:on (:command :spawn :from :head))
                                            (:interval (:from :heart :nth 6)))
                                           :include (supervisor-mixin)
                                           :do :invoke-with-event)
    (organ event)

  (cond ((and (eql (second event) :command)
              (getf event :command) :spawn)
         (spawn-agent behavior organ event))

        ((eql (second event) :beat)
         (children-check behavior organ))))
