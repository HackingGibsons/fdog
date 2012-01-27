(in-package :zmq)
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
