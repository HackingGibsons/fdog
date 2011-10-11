(in-package :fdog-control)

;; Helper functions
(defun header-json-type ()
  '("Content-Type" . "application/json"))

;; Condition handling
(defgeneric handle-http-condition (condition handler request raw)
  (:documentation "Generic for handling HTTP conditions. Specific implementations are defined in the def-http-code macro."))

(define-condition http-error-condition () ())

(defmacro def-http-code (code desc &key default)
  "Macro for functionality related to HTTP status codes. Currently creates a condition and API handler.
'code' - the numerical status code (404, 500...)
'desc' - the description of the status code (\"Not found\", \"Internal server error\")
'default' - A format string for a default error message to be encoded in the JSON"

  `(progn
     (define-condition ,(intern (string-upcase (format nil "~{~A~}" (list code "-condition")))) (http-error-condition)
       ((data :initform ,desc
              :initarg :data
              :reader ,(intern (string-upcase (format nil "~{~A~}" (list code "-data"))))))
       (:report (lambda (c s)
                  (format s ,(string-upcase (format nil "~{~A~}" (list code " Raised: ~A"))) (,(intern (string-upcase (format nil "~{~A~}" (list code "-data")))) c)))))

     (defmethod handle-http-condition ((condition ,(intern (string-upcase (format nil "~{~A~}" (list code "-condition"))))) handler request raw)
       (declare (ignorable raw))
       (with-slots (data) condition
         (with-chunked-stream-reply (handler request stream
                                             :code ,code :status ,desc
                                             :headers ((header-json-type)))
           (log-for (trace) "[Condition] ~A" condition)
           ,(if default
                `(json:encode-json `((:error . ,(format nil ,@default))) stream)
                `(json:encode-json `((:error . ,(format nil "~A" data))) stream)))))
     (export (find-symbol ,(string-upcase (format nil "~{~A~}" (list code "-condition"))) ':fdog-control) ':fdog-control)))

(def-http-code 400 "Bad request")
(def-http-code 404 "Not found" :default ("Endpoint ~A not found." (api-subpath request)))
(def-http-code 500 "Internal server error")
