(in-package :afdog-utils)

(defun zmq-connect-and-dump (socktype addr &rest keys &key &allow-other-keys)
  (zmq:with-context (ctx 1)
    (zmq:with-socket (sock ctx socktype)
      (zmq:connect sock addr)

      (when (getf keys :subscribe)
        (zmq:setsockopt sock zmq:subscribe (getf keys :subscribe)))

      (let ((msg (make-instance 'zmq:msg)))
        (log-for (trace) "Starting message read...")
        (loop while (zmq:recv! sock msg) do
             (log-for (trace) "Message(~A): [~A]" (zmq:msg-size msg) (zmq:msg-data-as-string msg)))))))



