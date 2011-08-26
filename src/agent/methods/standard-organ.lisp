(in-package :agent)

;; Agent linkage methods
(defmethod agent-tick ((organ standard-organ) event)
  "By default, an organ tick is a no-op"
  nil)

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

(defmethod agent-disconnect :before ((agent standard-agent) (organ standard-organ) &rest options)
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
