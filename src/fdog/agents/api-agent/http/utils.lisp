(in-package :http-dog)

(defun header-json-type ()
  '("Content-Type" . "application/json"))

(defun decode-json-from-request (request)
  "Attempts to decode JSON from a mongrel2 request.
If there is an error parsing the JSON, throws a 400 error."
  (flet ((signal-400-error () (error '400-condition :details "JSON is malformed")))
    (handler-case
        (json:decode-json-from-string request)
      (end-of-file (cond) (signal-400-error))
      (json:json-syntax-error (cond) (signal-400-error)))))

(defun api-subpath (request)
  (let* ((prefix "/api"))
    (ppcre:regex-replace (format nil "^~A" prefix)
                         (m2cl:request-path request) "")))

(defun merge-headers (headers)
  (let ((default '((:code . 200) (:status . "OK")
                   ("Content-Type" . "text/html")
                   ("X-Fdog" . "afdog"))))
    (remove-duplicates (append default (remove-if #'null headers :key #'cdr))
                       :key #'car :test #'string=)))

;; Helpers
(defmacro with-chunked-stream-reply ((handler request stream &key code status headers) &body body)
  (let ((g!handler (gensym "handler"))
        (g!request (gensym "request"))
        (g!m2-handler (gensym "m2-handler"))
        (g!headers (gensym "headers")))
    `(let* ((,g!handler ,handler) (,g!request ,request)
            (,g!m2-handler ,g!handler)
            (,g!headers (merge-headers (list (cons :code ,code) (cons :status ,status)
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

;; Condition handling
(defgeneric handle-http-condition (condition agent organ handler request raw)
  (:documentation "Generic for handling HTTP conditions. Specific implementations are defined in the def-http-code macro."))

(define-condition http-error-condition ()
  ((code
    :initarg :code
    :reader code
    :documentation "The numerical status code")
   (status
    :initarg :status
    :reader status
    :documentation "A text description of the status code (e.g. \"Not Found\" for 404)")
   (details
    :initarg :details
    :initform nil
    :reader details
    :documentation "A detailed error message for the condition."))

  (:report (lambda (c s)
             ;; TODO Prints message in parentheses if non-nil (but
             ;; it's kind of a pain)
             (format s "~A condition raised: ~A" (code c) (status c))))
  (:documentation "Generic HTTP condition"))

(defmethod handle-http-condition ((condition http-error-condition) (agent api-agent) organ handler request raw)
  (declare (ignorable raw))
  (with-slots (status) condition
    (let ((code (code condition))
          (status (status condition)))
      (with-chunked-stream-reply (handler request stream
                                          :code code
                                          :status status
                                          :headers ((header-json-type)))
        (log-for (trace) "[Condition] ~A" condition)
        (let ((error-json (list (cons :error (format nil "~A" status)))))
          (awhen (details condition)
            (appendf error-json (list (cons :details it))))
          (json:encode-json error-json stream))))))

(defmacro def-http-code (code status)
  "Macro for functionality related to HTTP status codes. Currently creates a condition and API handler.
`code' - the numerical status code (404, 500...)
`status' - the description of the status code (\"Not found\", \"Internal server error\")
`default' - A format string for a default error message to be encoded in the JSON"
  (let* ((condition-name (string-upcase (format nil "~{~A~}" (list code "-condition"))))
         (condition-sym (intern condition-name)))

    `(progn
       (define-condition ,condition-sym (http-error-condition)
         ()
         (:default-initargs
          :code ,code
           :status ,status))

       (export (find-symbol ,condition-name ':http-dog) ':http-dog))))

(def-http-code 400 "Bad request")
(def-http-code 403 "Forbidden")
(def-http-code 404 "Not found")
(def-http-code 500 "Internal server error")
(def-http-code 504 "Gateway timeout")
