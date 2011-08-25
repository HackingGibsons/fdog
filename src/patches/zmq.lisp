(in-package :zmq)

(export 'send!)
(defun send! (sock msg &optional flags (count 0))
  "Keep trying to `zmq:send' while it keeps returning -1 with an errno
of EINTR recursively.  Second value returned is the number of times the operation was retried."
  (let* ((res (zmq:send sock msg flags))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eintr))
                     (send! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))

(export 'recv!)
(defun recv! (sock msg &optional flags (count 0))
  "Keep trying to `zmq:recv' while it keeps returning -1 with an errno
of EINTR or EFSM recursively.  Second value returned is the number of times the operation was retried."
  (let* ((res (zmq:recv sock msg flags))
         (res (cond ((and (= res -1)
                          (member (sb-alien:get-errno) `(,sb-posix:eintr ,zmq:efsm)))
                     (recv! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))
