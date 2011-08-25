(in-package :agent)

(defmethod agent-tick ((organ standard-organ) event)
  "By default, an organ tick is a no-op"
  nil)

(defmethod agent-tick ((heart agent-heart) e)
  (declare (ignorable e))
  (let ((now (get-internal-real-time)))
    (when (>= now (heart-next-beat heart))
      (zmq:with-socket (esock (agent-context (organ-agent heart)) zmq:pub)
        (zmq:connect esock (agent-event-addr (organ-agent heart)))
        (zmq:send! esock (prepare-message `(:heart :beat :uuid ,(organ-uuid heart) :time ,now))))

      (setf (heart-last-beat heart) now
            (heart-next-beat heart) (round (+ now (* (heart-beat-every heart)
                                                     internal-time-units-per-second)))))
    (heart-next-beat heart)))


;; Makers
(defmethod make-heart-for ((agent standard-agent))
  (let ((heart (make-instance 'agent-heart :agent agent)))
    heart))

(defun make-agent ()
  "Agent maker wrapper"
  (let* ((agent (make-instance 'standard-agent))
         (heart (make-heart-for agent)))
    (agent-connect agent heart)
    agent))

;; Agent methods
(defmethod agent-connect ((agent standard-agent) organ &rest options)
  (log-for (trace) "Connecting organ: ~A to agent: ~A With: ~A" organ agent options)
  (push organ (agent-organs agent))
  (values agent organ))


(defmethod initialize-instance :after ((agent standard-agent) &rest initargs)
  "Set up the interal bus address."
  (declare (ignorable initargs))
  (log-for (trace) "Setting the event-addr to inproc://uuid")
  (setf (slot-value agent 'event-addr)
        (format nil "inproc://~A" (agent-uuid agent)))
  (log-for (trace) "Setting the event-addr to inproc://msg-uuid")
  (setf (slot-value agent 'message-addr)
        (format nil "inproc://msg-~A" (agent-uuid agent))))

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

(defmethod next-event ((agent standard-agent))
  "Returns the next event pending for `agent' on the internal bus
or `:timeout' if no event is found after a pause."
  (log-for (trace) "Fetching event for: ~A" agent)
  (flet ((s2us (s) (round (* s 1000000)))

         (read-message ()
           (log-for (trace) "Reading message.")
           (let ((msg (make-instance 'zmq:msg)))
             (zmq:recv! (agent-event-sock agent) msg)
             (zmq:msg-data-as-string msg))))


    (zmq:with-polls ((readers . (((agent-event-sock agent) . zmq:pollin))))
      (let ((poll-result (zmq:poll readers :timeout (s2us (agent-poll-timeout agent)) :retry t)))
        (if poll-result
            (read-message)
            :timeout)))))


(defmethod event-fatal-p ((agent standard-agent) event)
  "Predicate to determine if this event should end the agent."
  (log-for (trace) "Testing event fatalaty of ~A for ~A" event agent)
  (if (and (event-timeout-p event)
           (> (- (get-internal-real-time) (agent-last-event agent))
              (* *event-starvation-timeout* internal-time-units-per-second)))
      (prog1 t (log-for (warn) "Event timeout ~As reached." *event-starvation-timeout*))
      (not event)))

(defmethod run-agent ((agent standard-agent))
  "Enter the agent event loop, return only when agent is dead."
  (zmq:with-context (ctx *context-threads*)
    (zmq:with-socket (event-sock ctx zmq:sub)
      (zmq:with-socket (message-sock ctx zmq:pub)
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
        (do ((event (next-event agent) (next-event agent)))
            ((or (not event)
                 (event-fatal-p agent event))
             event)
          (log-for (trace) "Agent[~A] Event: ~A" agent event)

          ;; Tick, Process, Increment counter
          (agent-tick agent event)
          (act-on-event agent event)
          (incf (agent-event-count agent)))

        (log-for (trace) "Agent exiting: ~A. ~A events processed" agent (agent-event-count agent))))))

(defgeneric prepare-message (message)
  (:method (message)
    (make-instance 'zmq:msg :data (with-output-to-string (s) (prin1 message s))))
  (:method ((message string))
    (make-instance 'zmq:msg :data message)))

(defgeneric agent-publish-event (agent event)
  (:method ((agent standard-agent) event)
    (log-for (trace) "Publishing event: [~A]" (with-output-to-string (s) (prin1 event s)))
    (zmq:with-socket (esock (agent-context agent) zmq:pub)
        (zmq:connect esock (agent-event-addr agent))
        (zmq:send! esock (prepare-message event)))))

(defgeneric agent-send-message (agent event)
  (:method ((agent standard-agent) event)
    (log-for (trace) "Sending message: [~A]" (with-output-to-string (s) (prin1 event s)))
    (zmq:send! (agent-message-sock agent) (prepare-message event))))

(defmethod act-on-event ((agent standard-agent) event)
  "Perform any action an `agent' would need to take to act on `event'"
  (log-for (trace) "Agent: ~A processing event: ~A" agent event)

  ;; Rebroadcast useful events
  (unless (or (not event) (event-timeout-p event))
    (agent-send-message agent event)))

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
