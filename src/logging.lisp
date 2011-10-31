(in-package :afdog)

(log5:defoutput human-time (multiple-value-bind (second minute hour date month year)
                               (decode-universal-time (get-universal-time))
                             (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

;;; Logging functions for zmq senders
(defun start-logging (&key (default t))
  (when default
    (log5:start-sender 'default
                       (log5:stream-sender :location (make-instance 'zmq-logging-stream))
;                       :category-spec '(log5:dribble+)
                       :category-spec '(log5:warn+)
                       :output-spec '(human-time log5:category log5:message))))

(defun stop-logging ()
  (log5:stop-all-senders))

(defclass zmq-logging-stream (fundamental-character-output-stream)
  ((ctx
     :accessor ctx
     :documentation "The zeromq context")
   (socket
     :accessor socket
     :documentation "The zeromq socket")
   (address
     :initform "tcp://127.0.0.1:5555"
     :accessor address
     :documentation "The address to send log messages to")))

(defmethod initialize-instance :after ((stream zmq-logging-stream) &key)
  (with-slots (ctx socket address) stream
    (setf ctx (zmq:init 1))
    (setf socket (zmq:socket ctx zmq:push))
    (zmq:connect socket address)))

(defmethod close ((stream zmq-logging-stream) &key abort)
  (with-slots (ctx socket) stream
    (zmq:close socket)
    (zmq:term ctx)))

(defmethod stream-write-char ((stream zmq-logging-stream) char)
  (stream-write-sequence stream
    (with-output-to-string (cheating) (format cheating "~C" char))))

(defmethod stream-write-string ((stream zmq-logging-stream) str &optional start end)
  (stream-write-sequence stream (subseq str start end)))

(defmethod stream-write-sequence ((stream zmq-logging-stream) seq &optional start end)
  (with-slots (socket) stream
    (let ((msg (make-instance 'zmq:msg :data (subseq seq (or start 0) end))))
      (zmq:send! socket msg))))

