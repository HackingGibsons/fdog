(in-package :fdog-handler)

(defclass chunked-http-output-stream (fundamental-output-stream fundamental-character-stream)
  ((handler
    :initarg :handler
    :accessor chunked-stream-handler
    :documentation "The m2cl handler instance to use for writing")
   (request
    :initarg :request
    :accessor chanked-stream-request))
  (:documentation "A stream for providing chunked HTTP output"))

(defmethod close ((stream chunked-http-output-stream) &key abort)
  (with-slots (handler request) stream
    (m2cl:handler-send-http-chunked-finish responder-handler :request request)))

(defmethod stream-write-char ((stream chunked-http-output-stream) char)
  (stream-write-sequence stream
   (with-output-to-string (cheating) (format cheating "~C" char))))

(defmethod stream-write-string ((stream chunked-http-output-stream) str &optional start end)
  (stream-write-sequence stream (subseq str start end)))

(defmethod stream-write-sequence ((stream chunked-http-output-stream) seq &optional start end)
  (with-slots (handler request) stream
    (m2cl:handler-send-http-chunk handler (subseq seq (or start 0) end) :request request)))

