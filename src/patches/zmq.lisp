(in-package :zmq)

;; New socktypes
(export '(router dealer))
(defconstant router xrep)
(defconstant dealer xreq)

;; New constants
(export '(fd events reconnect-ivl reconnect-ivl-max))
(defconstant fd 14)
(defconstant events 15)
(defconstant reconnect-ivl 18)
(defconstant reconnect-ivl-max 21)

;; TODO: Import this back into the ZMQ module
;; Changes (so far):
;;  * Included reconnect-ivl* to the integer options
;;    Couldn't factor it out because the constant is defined here
(defun setsockopt (socket option value)
  (etypecase value
    (string (with-foreign-string (string value)
              (%setsockopt socket option string (length value))))
    (integer (cond
               ((member option (list linger reconnect-ivl-max reconnect-ivl) :test #'=)
                (with-foreign-object (int :int)
                  (setf (mem-aref int :int) value)
                  (%setsockopt socket option int (foreign-type-size :int))))
               (t
                (with-foreign-object (int :int64)
                  (setf (mem-aref int :int64) value)
                  (%setsockopt socket option int (foreign-type-size :int64))))))))

(export 'send!)
(defun send! (sock msg &optional flags (count 0))
  "Keep trying to `zmq:send' while it keeps returning -1 with an errno
of EINTR recursively.  Second value returned is the number of times the operation was retried."
  (let* ((res (handler-case (zmq:send sock msg flags) (simple-error () -1)))
         (res (cond ((and (= res -1)
                          (or (= (sb-alien:get-errno) sb-posix:eintr)
                              (= (sb-alien:get-errno) sb-posix:eagain)))
                     (send! sock msg flags (1+ count)))

                    (:otherwise
                     res))))
    (values res count)))

(export 'recv!)
(defun recv! (sock msg &optional flags (count 0))
  "Keep trying to `zmq:recv' while it keeps returning -1 with an errno
of EINTR or EFSM recursively.  Second value returned is the number of times the operation was retried."
  (let* ((res (handler-case (zmq:recv sock msg flags) (simple-error () -1)))
         (res (cond ((and (= res -1)
                          (member (sb-alien:get-errno) `(,sb-posix:eintr ,zmq:efsm)))
                     (recv! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))
