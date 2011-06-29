(in-package :fdog-forwarder)

(defmethod queue-request ((endpoint forwarder-queue-endpoint) msg)
  "Enqueue message on the endpoint to the current connected redis instance."
  (log-for (warn) "TODO: queue-request WIP")
  (log-for (trace) "Into redis for ~A: [~A]" endpoint (flex:octets-to-string msg))
  t)

(defmethod make-request-device ((endpoint forwarder-queue-endpoint))
  "Make a request device that pumps requests into redis."
  #'(lambda ()
      (log-for (trace) "Starting request queue device.")
      (redis:with-recursive-connection (:host (queue-endpoint-redis-host endpoint)
                                        :port (queue-endpoint-redis-port endpoint))
        (let ((msg (make-instance 'zmq:msg)))
          (labels ((run-once ()
                     (zmq:recv (endpoint-proxy-sock endpoint) msg)
                     (queue-request endpoint (zmq:msg-data-as-array msg)))

                   (handle-condition (c)
                     (or (= (sb-alien:get-errno) sb-posix:eintr)
                         (prog1 nil
                           (log-for (warn) "Queue device exited with condition: ~A" c)
                           (signal c))))

                   (run-device ()
                     (handler-case (run-once)
                       (simple-error (c) (handle-condition c)))))

            (loop while (run-device) do ':nothing))))))
