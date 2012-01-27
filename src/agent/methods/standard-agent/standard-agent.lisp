(in-package :agent)

;; Agent methods
(defmethod agent-connect ((agent standard-agent) organ &rest options)
  (declare (ignorable options))
  (log-for (trace) "Connecting organ: ~A to agent: ~A With: ~A" organ agent options)
  (push organ (agent-organs agent))
  (values agent organ))

(defmethod agent-boot ((agent standard-agent) organ &rest options)
  (declare (ignorable options))
  (log-for (trace) "Agent boot/NOP of: ~A for ~A" organ agent)
  nil)

(defmethod agent-disconnect ((agent standard-agent) organ &rest options)
  (declare (ignorable options))
  (log-for (trace) "Disconnecting/NOP for organ: ~A from ~A" organ agent)
  (log-for (trace) "Organs before: ~A" (agent-organs agent))
  (setf (agent-organs agent) (delete organ (agent-organs agent)))
  (log-for (trace) "Organs after: ~A" (agent-organs agent))
  (values agent organ))


(defmethod initialize-instance :after ((agent standard-agent) &rest initargs)
  "Set up the interal bus address."
  (declare (ignorable initargs))
  (log-for (trace) "Setting the event-addr to inproc://uuid")
  (setf (slot-value agent 'event-addr)
        (format nil "inproc://~A" (agent-uuid agent)))
  (log-for (trace) "Setting the event-addr to inproc://msg-uuid")
  (setf (slot-value agent 'message-addr)
        (format nil "inproc://msg-~A" (agent-uuid agent)))
  (initialize-instance-organs agent))

(defmethod agent-poll-timeout ((agent standard-agent))
  "Determine how long the poll timeout should be for the current poll
for `agent'"
  (log-for (trace) "Calculating timeout. Cron: ~A" (agent-cron agent))
  (let* ((now (get-internal-real-time))
         (soon (pop (agent-cron agent)))
         (soon (and soon
                    (/ (- soon now) internal-time-units-per-second)))
         (soon (and soon (if (>= soon 0) soon 0)))
         (timeout (if soon soon
                      (/ *event-starvation-timeout* 3))))

    (setf (agent-cron agent)
          (remove-if #'(lambda (c) (< c now)) (agent-cron agent)))

    (log-for (trace) "Using poll timeout of: ~Fs" timeout)
    timeout))

(defmethod next-event% ((agent standard-agent))
  "Returns the next event pending for `agent' on the internal bus
or `:timeout' otherwise after a scheduled pause. Will also poll
all of the organs and deliver any internal messages from the bus as well
as fire any callbacks that may be pending IO when it is ready."
  (let ((callbacks (make-hash-table))
        (agent-event :timeout))
    (labels ((s2us (s)
               "Seconds to uSeconds"
               (round (* s 1000000)))

             (read-agent-event (s)
               "Read and store the agent event. Used as the `agent-event-sock' callback"
               (setf agent-event
                     (afdog:read-message s)))

             (organ-readers+store-callbacks ()
               "Return a list of organ reader sockets and fill in the callbacks in the HT"
               (alexandria:flatten
                (mapcar #'(lambda (organ)
                            (multiple-value-bind (socks funs) (reader-callbacks organ)
                              (prog1 socks
                                (mapc #'(lambda (sock fun) (setf (gethash sock callbacks) fun))
                                      socks funs))))
                        (agent-organs agent)))))

      ;; TODO: Magic happens

      agent-event)))

(defmethod next-event ((agent standard-agent))
  "Returns the next event pending for `agent' on the internal bus
or `:timeout' otherwise after a scheduled pause. Will also poll
all of the organs and deliver any internal messages from the bus as well
as fire any callbacks that may be pending IO when it is ready."
  (labels ((s2us (s)
             "Seconds to uSeconds"
             (round (* s 1000000)))

           (make-reader (sock)
             "Wrap `sock' in a `zmq:pollitem' instance for `zmq:pollin' event"
             (make-instance 'zmq:pollitem :socket sock :events zmq:pollin))

           (organ-readers-and-callbacks ()
             "Returns two lists. A list of pollitems for read events and the callbacks for each."
             (do* ((organs (agent-organs agent) (rest organs))
                   (organ (car organs) (car organs))
                   readers callbacks)
                  ((not organ) (values (mapcar #'make-reader readers)
                                       callbacks))
               (multiple-value-bind (socks funs) (reader-callbacks organ)
                 (setf readers `(,@readers ,@socks)
                       callbacks `(,@callbacks ,@funs)))))

           (maybe-fire-callback (result poller callback)
             "Fires the given `callback' with the socket from the `poller' if `result' is 1"
             (when (equal result 1)
               (funcall callback (zmq:pollitem-socket poller)))))

    (multiple-value-bind (callback-readers callbacks) (organ-readers-and-callbacks)
      (let* ((readers `(,(make-reader (agent-event-sock agent)) ;; Agent event socket
                         ,@callback-readers))
             (poll (zmq:poll readers :timeout (s2us (agent-poll-timeout agent)) :retry t)))


        (mapc #'maybe-fire-callback
              (rest poll) (rest readers) callbacks)

        (if (equal (first poll) 1)
            (afdog:read-message (agent-event-sock agent))
            :timeout)))))


(defmethod event-fatal-p ((agent standard-agent) event)
  "Predicate to determine if this event should end the agent."
  (log-for (trace) "Testing event fatalaty of ~A[~A] for ~A" event (type-of event) agent)
  (let ((parsed (and (typep event 'string) (handler-case (read-from-string event) (end-of-file () nil)))))
    (cond ((not event)
           (log-for (warn) "Not an event! Considering fatal.")
           t)

          ((and (event-timeout-p event)
                (> (- (get-internal-real-time) (agent-last-event agent))
                   (* *event-starvation-timeout* internal-time-units-per-second)))
           (prog1 t
             (log-for (warn) "Event timeout ~As reached." *event-starvation-timeout*)))

          ((eql (getf parsed :command) :die)
           (prog1 t
             (log-for (warn) "Suicide event: ~A" event))))))

(defmethod run-agent :around ((agent standard-agent))
  (handler-case (call-next-method)
    (t (c)
      (let ((output (make-pathname :directory `(:absolute "tmp") :type "error"
                                   :name (format nil "~A-~A.~A" (type-of agent) (agent-uuid agent) (iolib.syscalls:getpid)))))

        (log-for (warn) "Agent: [~A/~A] Event loop exited poorly: ~A" (type-of agent) (agent-uuid agent) c)
        (handler-case
            (with-open-file (s output :if-does-not-exist :create :if-exists :append :direction :output)
              (format s "~&=Crash report for ~A/~A=~%" (type-of agent) (agent-uuid agent))
              (format s "~&==Condition==~%")
              (format s "~&Type: ~A~%" (type-of c))
              (trivial-backtrace:print-condition c s)
              (format s "~&==Condition End==~%")
              (format s "~&==Backtrace==~%")
              (trivial-backtrace:print-backtrace-to-stream s)
              (format s "~&==Backtrace End==~%")
              (format s "~&=Crash report end=~%")
              (log-for (warn) "Crash report written to: ~A" (namestring output)))

          (t (write-error)
            (log-for (warn) "Error writing crash report to: ~A: ~A" (namestring output) write-error)))))))



(defmethod run-agent ((agent standard-agent))
  "Enter the agent event loop, return only when agent is dead."
  (zmq:with-context (ctx *context-threads*)
    (zmq:with-socket (event-sock ctx zmq:sub)
      (zmq:setsockopt event-sock zmq:linger *socket-linger*)
      (zmq:with-socket (message-sock ctx zmq:pub)
        (zmq:setsockopt message-sock zmq:linger *socket-linger*)
        (log-for (trace) "Binding event sock to: ~A" (agent-event-addr agent))
        (zmq:bind event-sock (agent-event-addr agent))
        (log-for (warn) "Subscribing event sock to everyting")
        (zmq:setsockopt event-sock zmq:subscribe "")

        (log-for (trace) "Binding message sock to: ~A" (agent-message-addr agent))
        (zmq:bind message-sock (agent-message-addr agent))

        (log-for (trace) "Setting agent context and event sock: ~A" agent)
        (setf (slot-value agent 'context) ctx
              (slot-value agent 'event-sock) event-sock
              (slot-value agent 'message-sock) message-sock)

        ;; Send boot
        (agent-publish-event agent `(:boot ,(get-internal-real-time) :uuid ,(agent-uuid agent)))

        ;; Agent event loop
        (setf (agent-last-event agent) (get-internal-real-time))
        (setf (agent-event-count agent) 0)
        (log-for (trace) "Entering agent event loop.")
        (unwind-protect
             (do ((event (next-event agent) (next-event agent)))
                 ((event-fatal-p agent event)
                  event)
               (log-for (trace) "Agent[~A] Event: ~A" agent event)

               ;; Process, Tick, Increment counter
               ;; Event has to be acted on before the tick fires
               ;; Because actions can alter the reactor before the tick
               ;; Operates
               (act-on-event agent event)
               (agent-tick agent event)
               (incf (agent-event-count agent)))

          ;; Event loop unwind
          (flet ((organ-disconnect (o) (agent-disconnect agent o)))
            (log-for (warn) "Disconnecting organs.")
            (mapcar #'organ-disconnect (agent-organs agent))
            (log-for (warn) "Organs disconnected.")))

        (log-for (warn) "Agent exiting: ~A. ~A events processed" agent (agent-event-count agent))))))

(defgeneric prepare-message (message)
  (:method (message)
    (prepare-message (with-output-to-string (s) (prin1 message s))))
  (:method ((message string))
    (make-instance 'zmq:msg :data message)))

(defgeneric agent-publish-event (agent event)
  (:method ((agent standard-agent) event)
    (log-for (trace) "Publishing event: [~A]" (with-output-to-string (s) (prin1 event s)))
    (zmq:with-socket (esock (agent-context agent) zmq:pub)
      (zmq:setsockopt esock zmq:linger *socket-linger*)
      (zmq:connect esock (agent-event-addr agent))
      (zmq:send! esock (prepare-message event)))))

(defgeneric agent-send-message (agent event)
  (:method ((agent standard-agent) event)
    (log-for (trace) "Sending message: [~A]" (with-output-to-string (s) (prin1 event s)))
    (zmq:send! (agent-message-sock agent) (prepare-message event))))

(defmethod agent-event-special-p ((agent standard-agent) event)
  (and (consp event)
       (or (eql (car event) :boot))))

(defgeneric agent-special-event (agent head event)
  (:documentation "Method responsible for handling internal special events like boot")
  (:method ((agent standard-agent) (head (eql :boot)) event)
    (log-for (trace) "Handling the boot event!")
    (flet ((organ-boot (o) (agent-boot agent o)))
      (mapcar #'organ-boot (agent-organs agent)))))

(defmethod act-on-event ((agent standard-agent) event)
  "Perform any action an `agent' would need to take to act on `event'"
  (log-for (trace) "Agent: ~A processing event(~A): ~A" agent (type-of event) event)

  (let ((event (typecase event
                 (string (handler-case (read-from-string event) (end-of-file () nil)))
                 (otherwise event))))
    (unless event
      (log-for (warn) "Not an event to act on!")
      (return-from act-on-event))

    (cond ((agent-event-special-p agent event)
           (agent-special-event agent (car event) event))

          ((not (event-timeout-p event))
           (agent-send-message agent event)))))

(defun event-timeout-p (event)
  "Predicate testing if the `event' represents a timeout event"
  (equal :timeout event))

(defmethod agent-tick ((agent standard-agent) event)
  "Internal tick, measure time, queue timed events, update timeout clock."
  (let ((last (agent-last-tick agent))
        (now (get-internal-real-time)))

    (flet ((organ-tick (o) (agent-tick o event)))
      ;; Compile a list of maximum times any organs want the next local tick to happen
      ;; to compute the correct poll timeout
      (let* ((maybe-crons (remove nil (mapcar #'organ-tick (agent-organs agent))))
             (maybe-crons (append maybe-crons (agent-cron agent)))
             (maybe-crons (remove-duplicates maybe-crons :test #'=))
             (maybe-crons (sort maybe-crons #'<)))
        (setf (agent-cron agent) maybe-crons)))


    (unless (event-timeout-p event)
      (log-for (trace) "Not a timeout event.")
      (setf (agent-last-event agent) now))

    (setf (agent-tick-delta agent) (- now last)
          (agent-last-tick agent) now)

    (log-for (trace) "Agent ~A tick. Tick Delta: ~A" agent (agent-tick-delta agent))))

(defmethod agent-info ((organ standard-organ))
  "An organ specific information snippet. Collected by
`agent-info' on `standard-agent'. nil by default."
  (log-for (trace) "~A did not define additional info." organ)
  nil)

(defmethod agent-provides ((agent standard-agent))
  nil)

(defmethod agent-info ((agent standard-agent))
  (let ((provides (agent-provides agent)))
    (append `(:uuid ,(agent-uuid agent) :type ,(type-of agent)
              :container (:pid ,(iolib.syscalls:getpid) :thread ,(princ-to-string (bt:current-thread)))
              :timestamp ,(get-universal-time) :age ,(age agent)
              ,@(when provides
                  (list :provides provides)))
            (loop for organ in (agent-organs agent)
               appending (agent-info organ)))))

;; Utils
;; TODO: Extract these to a file
(defmethod find-organ (agent tag)
  (find tag (agent-organs agent) :key #'organ-tag))

(defparameter *ipc-sock-prefix* "ipc:///tmp/agent.sock")

(defmethod local-ipc-addr ((agent standard-agent) &optional organ-tag)
  (local-ipc-addr (agent-uuid agent) organ-tag))

(defmethod local-ipc-addr ((agent-uuid string) &optional organ-tag)
  (format nil "~A.~A~@[.~A~]" *ipc-sock-prefix* agent-uuid organ-tag))

(defmethod age ((agent standard-agent))
  "Returns the age of the agent in `internal-time-units'"
  (- (get-internal-real-time) (start-time agent)))

(defmethod younger-p ((agent standard-agent) other-age)
  "Returns true if the agent is younger than the given age."
  (minusp (- (age agent) other-age)))

(defun correct-age (age timestamp)
  "Uses the announced timestamp to determine what the age should really be.
If a message was sent 3 seconds ago and was just now picked up for processing, the age should have 3 seconds added to it.
Otherwise the age will be 3 seconds too old and may get the wrong agent killed."
  (let ((latency (- (get-universal-time) timestamp)))
    (+ age (* internal-time-units-per-second latency))))
