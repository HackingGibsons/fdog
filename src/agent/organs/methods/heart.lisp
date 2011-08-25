(in-package :agent)

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


