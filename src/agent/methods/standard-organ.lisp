(in-package :agent)

;; Agent linkage methods
(defmethod agent-boot ((agent standard-agent) (organ standard-organ) &rest options)
  (declare (ignorable options))
  (call-next-method)
  (log-for (trace) "Organ specific boot of ~A for ~A" organ agent)

  (log-for (trace) "Building and connecting sockets.")
  (setf (organ-incoming-sock organ)
        (zmq:socket (agent-context agent) zmq:sub)

        (organ-outgoing-sock organ)
        (zmq:socket (agent-context agent) zmq:pub))

  (log-for (trace) "Connecting the incoming socket: ~A" (agent-message-addr agent))
  (zmq:connect (organ-incoming-sock organ) (agent-message-addr agent))
  (log-for (warn) "Subscribing incoming socket to everything.")
  (zmq:setsockopt (organ-incoming-sock organ) zmq:subscribe "")

  (log-for (trace) "Connecting the outgoing socket: ~A" (agent-event-addr agent))
  (zmq:connect (organ-outgoing-sock organ) (agent-event-addr agent))

  organ)

(defmethod agent-disconnect :after ((agent standard-agent) (organ standard-organ) &rest options)
  (declare (ignorable options))
  (log-for (trace) "Organ specific :before disconnect of ~A from ~A" organ agent)

  (log-for (trace) "Closing outgoing sock.")
  (when (organ-outgoing-sock organ)
    (zmq:close (organ-outgoing-sock organ))
    (setf (organ-outgoing-sock organ) nil))

  (log-for (trace) "Closing incoming sock.")
  (when (organ-incoming-sock organ)
    (zmq:close (organ-incoming-sock organ))
    (setf (organ-incoming-sock organ) nil)))

;; Messaging
(defmethod send-message ((organ standard-organ) message)
  (log-for (trace) "Organ sending message: [~A]" message)
  (zmq:send! (organ-outgoing-sock organ) (prepare-message message)))


;; Event handling
(defmethod act-on-event ((organ standard-organ) event)
  "Default primary method for `standard-organ' so that the remainig machinery can function."
  (log-for (warn) "Organ: ~A doesn't handle events as: ~A" organ event))

(defmethod act-on-event :around ((organ standard-organ) event)
  "Process the event for consumption by the primary method chain by trying to read it into a cons."

  (log-for (trace) "Organ: ~A processing event(~A): ~A" organ (type-of event) event)

  (let ((parsed (typecase event
                  (string (handler-case (read-from-string event) (end-of-file () nil)))
                  (zmq:msg (handler-case (read-from-string (zmq:msg-data-as-string event)) (end-of-file () nil)))
                  (otherwise event))))
    (call-next-method organ parsed)))

(defmethod act-on-event :before ((organ standard-beating-organ) event)
  "Process any heart-beat events of a `standard-beating-organ' with magic."

  (log-for (trace) "Maybe replying to heartbeat for ~A" organ)

  (prog1 event
    (when (and (listp event) (eql (getf event :heart) :beat))
      (setf (last-beat organ) (getf event :time))
      (log-for (trace) "~A replying to heartbeat." organ)
      (send-message organ `(,(organ-tag organ) :beat
                             :uuid ,(organ-uuid organ)
                             :time ,(get-internal-real-time))))))
;; Hard ticks
(defmethod agent-tick ((organ standard-organ) event)
  "By default, an organ tick is a no-op"
  nil)

