(in-package :afdog)

(log5:defoutput human-time (multiple-value-bind (second minute hour date month year)
                               (decode-universal-time (get-universal-time))
                             (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

;;; Logging functions for zmq senders
(defun start-logging (&key (default t) (syslog t) (category '(log5:dribble+)))
  "Method to start an agent sending log messages.
Keys:
`default' - whether to send messages over zeromq for stdout (default t)
`syslog' - whether to send messages over syslog (info+) (default t)
`category' - list of log5 categories to send over zeromq (default '(log5:dribble+))"
  (when default
    (log5:start-sender 'default
                       (log5:stream-sender :location (make-instance 'zmq-logging-stream))
                       :category-spec category
                       :output-spec '(human-time log5:category log5:message)))
  (when syslog
    (log5:start-sender 'syslog-info
                       (log5:stream-sender :location (make-instance 'syslog-logging-stream :priority :info))
                       :category-spec '(log5:info)
                       :output-spec '(log5:message))
    (log5:start-sender 'syslog-warn
                       (log5:stream-sender :location (make-instance 'syslog-logging-stream :priority :warning))
                       :category-spec '(log5:warn)
                       :output-spec '(log5:message))
    (log5:start-sender 'syslog-error
                       (log5:stream-sender :location (make-instance 'syslog-logging-stream :priority :err))
                       :category-spec '(log5:error)
                       :output-spec '(log5:message))
    (log5:start-sender 'syslog-fatal
                       (log5:stream-sender :location (make-instance 'syslog-logging-stream :priority :crit))
                       :category-spec '(log5:fatal)
                       :output-spec '(log5:message))))

(defun stop-logging ()
  "Method to make an agent stop sending log messages."
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
     :documentation "The address to send log messages to"))
  (:documentation "A stream to send messages over zeromq."))

(defmethod initialize-instance :after ((stream zmq-logging-stream) &key)
  "Opens the appropriate zeromq socket."
  (with-slots (ctx socket address) stream
    (setf ctx (zmq:init 1))
    (setf socket (zmq:socket ctx :push))
    (zmq:setsockopt socket :linger *socket-linger*)
    (zmq:connect socket address)))

(defmethod close ((stream zmq-logging-stream) &key abort)
  "Cleans up the zeromq socket when stream is closed."
  (with-slots (ctx socket) stream
    (zmq:close socket)
    ))

(defmethod stream-write-char ((stream zmq-logging-stream) char)
  (stream-write-sequence stream
    (with-output-to-string (cheating) (format cheating "~C" char)) 0 nil))

(defmethod stream-write-string ((stream zmq-logging-stream) str &optional start end)
  (stream-write-sequence stream str start end))

(defmethod stream-write-sequence ((stream zmq-logging-stream) seq start end &key)
  (with-slots (socket) stream
    (zmq:send! socket (subseq seq (or start 0) end))))

;;; Logging functions for the zmq receiver to output to console/disk
(defcategory output)

;;; syslog stream
(defclass syslog-logging-stream (fundamental-character-output-stream trivial-gray-stream-mixin)
  ((priority
     :initarg :priority
     :initform (error "Syslog priority not specified")
     :accessor priority))
  (:documentation "A stream to send messages to syslog."))

(defmethod stream-write-char ((stream syslog-logging-stream) char)
  (stream-write-sequence stream
    (with-output-to-string (cheating) (format cheating "~C" char)) 0 nil))

(defmethod stream-write-string ((stream syslog-logging-stream) str &optional start end)
  (stream-write-sequence stream str start end))

(defmethod stream-write-sequence ((stream syslog-logging-stream) seq start end &key)
  (unless (string-empty seq)
    (syslog:log "afdog" :local7 (priority stream) (trim-whitespace (subseq seq (or start 0) end)) syslog:+log-pid+)))

(defun start-logging-collect (&key logfile (default t))
  "A method to start collecting zeromq log messages.
Keys:
`default' - whether to print to stdout (default t)
`logfile' - a logfile to write to (default nil)"
  (format t "Log collector started.")
  (when default
    (log5:start-sender 'default-collector
                       (log5:stream-sender :location *standard-output*)
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
    (zmq:with-socket (socket ctx :pull)
      (zmq:bind socket *socket-address*)
      (labels ((run-once ()
                 (let ((string (zmq:recv! socket :string)))
                   (unless (string-empty string)
                     (log-for (output) (trim-whitespace string))))
                 :always-run))
        (loop while (run-once) do ':nothing)))))

(defun trim-whitespace (string)
  "Trims leading and trailing whitespace from a string."
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun string-empty (string)
  "Test whether a string is empty after trimming whitespace."
  (or (= (length string) 0)
      (= (length (trim-whitespace string)) 0)))
