(in-package :fdog-forwarder)

(defmethod queue-request ((endpoint forwarder-queue-endpoint) msg)
  "Enqueue message on the endpoint to the current connected redis instance."
  (log-for (warn) "TODO: queue-request WIP")
  t)

(defmethod make-request-device ((endpoint forwarder-queue-endpoint))
  "Make a request device that pumps requests into redis."
  #'(lambda ()
      (log-for (trace) "Starting request queue device.")
      (let ((msg (make-instance 'zmq:msg)))
        (flet ((run-device ()
                 (handler-case
                     (progn
                       (zmq:recv (endpoint-proxy-sock endpoint) msg)
                       (queue-request endpoint (zmq:msg-data-as-array)))
                   (simple-error (c)
                     (if (= (sb-alien:get-errno) sb-posix:eintr)
                         t
                         (prog1 nil (log-for (warn) "Queue device exited with condition: ~A" c)
                                (signal c)))))))
          (loop while (run-device) do ':nothing)))))
