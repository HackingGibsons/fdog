(in-package :afdog)

(log5:defoutput human-time (multiple-value-bind (second minute hour date month year)
                               (decode-universal-time (get-universal-time))
                             (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

;;; Logging functions for zmq senders
(defun start-logging (&key (default t) (category '(log5:dribble+)))
  (when default
    (log5:start-sender 'default
                       (log5:stream-sender :location (make-instance 'zmq-logging-stream))
                       :category-spec category
                       :output-spec '(human-time log5:category log5:message))))

(defun stop-logging ()
  (log5:stop-all-senders))

(defclass zmq-logging-stream (fundamental-character-output-stream trivial-gray-stream-mixin)
  ((ctx
     :accessor ctx
     :documentation "The zeromq context")
   (socket
     :accessor socket
     :documentation "The zeromq socket")
   (address
     :initform *socket-address*
     :accessor address
     :documentation "The address to send log messages to")))

(defmethod initialize-instance :after ((stream zmq-logging-stream) &key)
  (with-slots (ctx socket address) stream
    (setf ctx (zmq:init 1))
    (setf socket (zmq:socket ctx zmq:push))
    (zmq:setsockopt socket zmq:linger *socket-linger*)
    (zmq:connect socket address)))

(defmethod close ((stream zmq-logging-stream) &key abort)
  (with-slots (ctx socket) stream
    (zmq:close socket)
    (zmq:term ctx)))

(defmethod stream-write-char ((stream zmq-logging-stream) char)
  (stream-write-sequence stream
    (with-output-to-string (cheating) (format cheating "~C" char)) 0 nil))

(defmethod stream-write-string ((stream zmq-logging-stream) str &optional start end)
  (stream-write-sequence stream str start end))

(defmethod stream-write-sequence ((stream zmq-logging-stream) seq start end &key)
  (with-slots (socket) stream
    (let ((msg (make-instance 'zmq:msg :data (subseq seq (or start 0) end))))
      (zmq:send! socket msg))))

;;; Logging functions for the zmq receiver to output to console/disk
(defcategory output)

(defun start-logging-collect (&key logfile (default t))
  (format t "Log collector started.")
  (when default
    (log5:start-sender 'default-collector
                       (log5:stream-sender :location *error-output*)
                       :category-spec '(output)
                       :output-spec '(log5:message)))

  (when (and logfile (typep logfile 'string))
    (log5:start-sender 'disk
                       (log5:stream-sender :location (merge-pathnames
                                                       (make-pathname :directory `(:relative ,*fdog-log-dir*)
                                                                      :name logfile :type "log")
                                                       *root-path*))
                       :category-spec '(output)
                       :output-spec '(log5:message)))
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx zmq:pull)
      (zmq:bind socket *socket-address*)
      (let ((msg (make-instance 'zmq:msg)))
        (labels ((trim-whitespace (string)
                   (string-trim '(#\Space #\Tab #\Newline) string))

                 (string-empty (string)
                   (or (= (length string) 0)
                       (= (length (trim-whitespace string)) 0)))

                 (run-once ()
                   (zmq:recv! socket msg)
                   (let ((string (zmq:msg-data-as-string msg)))
                     (unless (string-empty string)
                       (log-for (output) (trim-whitespace string))))
                   :always-run))
          (loop while (run-once) do ':nothing))))))
