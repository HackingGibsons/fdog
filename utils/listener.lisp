(in-package :afdog-utils)
#| Dead code
(defun zmq-connect-and-dump (socktype addr &rest keys &key (use-log t) &allow-other-keys)
  (zmq:with-context (ctx 1)
    (zmq:with-socket (sock ctx socktype)
      (zmq:setsockopt sock zmq:linger 250)
      (zmq:connect sock addr)

      (when (getf keys :subscribe)
        (zmq:setsockopt sock zmq:subscribe (getf keys :subscribe)))

      (let ((msg (make-instance 'zmq:msg)))
        (if use-log
            (log-for (trace) "Starting message read...")
            (format t "Starting message read...~%"))
        (loop while (zmq:recv! sock msg) do
             (if use-log
                 (log-for (trace) "Message(~A): [~A]" (zmq:msg-size msg) (zmq:msg-data-as-string msg))
                 (format t "Message(~A): [~A]~%" (zmq:msg-size msg) (zmq:msg-data-as-string msg))))))))
|#
