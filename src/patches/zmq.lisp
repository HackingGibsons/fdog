(in-package :zmq)

(export 'send!)
(defmethod send! (sock msg &optional flags (count 0))
  (let* ((res (handler-case (zmq:send sock msg flags) (zmq:zmq-error () -1)))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eagain))
                     (send! sock msg flags (1+ count)))

                    (:otherwise
                     res))))
    (values res count)))

(export 'recv!)
(defmethod recv! (sock msg &optional flags (count 0))
  (let* ((res (handler-case (zmq:recv sock msg flags) (zmq:zmq-error () -1)))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eagain))
                     (recv! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))
