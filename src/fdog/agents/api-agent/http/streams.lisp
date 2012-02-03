(in-package :http-dog)

;; Chunked stream definition
(defclass chunked-http-output-stream (fundamental-character-output-stream)
  ((handler
    :initarg :handler
    :accessor chunked-stream-handler
    :documentation "The m2cl handler instance to use for writing")
   (request
    :initarg :request
    :accessor chanked-stream-request))
  (:documentation "A stream for providing chunked HTTP output"))

(defmethod close ((stream chunked-http-output-stream) &key abort)
  (declare (ignorable abort))
  (with-slots (handler request) stream
    (m2cl:handler-send-http-chunked-finish handler :request request)))

(defmethod stream-write-char ((stream chunked-http-output-stream) char)
  (format stream "~C" char))

(defmethod stream-write-string ((stream chunked-http-output-stream) str &optional start end)
  (stream-write-sequence stream str start end))

(defmethod stream-write-sequence ((stream chunked-http-output-stream) seq start end &key)
  (with-slots (handler request) stream
    (m2cl:handler-send-http-chunk handler (subseq seq (or start 0) end) :request request)))

;; Helpers
(defmacro with-chunked-stream-reply ((handler request stream &key code status headers) &body body)
  (let ((g!handler (gensym "handler"))
        (g!request (gensym "request"))
        (g!m2-handler (gensym "m2-handler"))
        (g!headers (gensym "headers")))
    `(let* ((,g!handler ,handler) (,g!request ,request)
            (,g!m2-handler (request-handler-m2cl ,g!handler))
            (,g!headers (merge-headers (list '(:code . ,code) '(:status . ,status)
                                             ,@headers))))
       (with-open-stream (,stream (make-instance 'chunked-http-output-stream
                                                 :handler ,g!m2-handler :request ,g!request))
         ;; TODO: This should sit somewhere in the stream class?
         (m2cl:handler-send-http-chunked ,g!m2-handler :request ,g!request
                                         :code (cdr (assoc :code ,g!headers)) :status (cdr (assoc :status ,g!headers))
                                         :headers (remove-if (lambda (x) (member x '(:code :status)))
                                                             ,g!headers
                                                             :key #'car))
         ,@body))))
