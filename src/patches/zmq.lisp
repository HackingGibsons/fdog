(in-package :zmq)
(export 'poll-item-sock)

(defun poll-item-sock (poll-item)
  "Return the `socket' of the given `poll-item'"
  (foreign-slot-value poll-item 'pollitem 'socket))

(export 'send!)
(export 'recv!)

;; Helper declaration and export
(defgeneric send! (sock msg &optional flags count)
  (:documentation "A wrapper around the low level `zmq:send'
supporting wrapping native types in ephemeral messages for transport."))
(defgeneric recv! (sock msg &optional flags count)
  (:documentation "A wrapper around the low level `zmq:recv'"))

;; Type-specific helpers
(defmethod send! (sock (seq sequence) &optional flags count)
  "Translate a sequence into a 0MQ message and forward the method call."
  (zmq:with-msg-init-data (msg seq)
    (send! sock msg flags count)))

(defmethod recv! (sock (msg (eql :string)) &optional flags count)
  "Receive a message from `sock' and return the contents as a string."
  (zmq:with-msg-init (msg)
    (multiple-value-bind (r c) (recv! sock msg flags count)
      (declare (ignore r))
      (values (zmq:msg-data-string msg) c))))

(defmethod recv! (sock (what (eql :msg)) &optional flags count)
  "Receive and return the raw message object.

!!WARNING!!: The returned value needs to be released with `zmq:msg-close'
when finished with to avoid leaking in foreign code."
  (declare (ignore what))
  (let ((msg (zmq:msg-init)))
    (handler-case (prog1 msg (recv! sock msg flags count))
      (t (c)
        ;; In case of fire, clean up and keep panicing up the stack
        (zmq:msg-close msg)
        (error c)))))

;; Actual low-level interfacing methods.
(defmethod send! (sock msg &optional flags (count 0))
  (let* ((res (handler-case (zmq:send sock msg flags) (zmq:zmq-error () -1)))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eagain))
                     (send! sock msg flags (1+ count)))

                    (:otherwise
                     res))))
    (values res count)))

(defmethod recv! (sock msg &optional flags (count 0))
  (let* ((res (handler-case (zmq:recv sock msg flags) (zmq:zmq-error () -1)))
         (res (cond ((and (= res -1)
                          (= (sb-alien:get-errno) sb-posix:eagain))
                     (recv! sock msg flags (1+ count)))

                    (:otherwise res))))
    (values res count)))
