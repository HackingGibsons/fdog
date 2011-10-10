(in-package :fdog-control)

(defparameter *api-version* 1)

(defgeneric handle-http-condition (condition handler request raw)
  (:documentation "Generic for handling HTTP conditions. Specific implementations are defined in the def-http-code macro."))

(define-condition http-error-condition () ())

(defmacro def-http-code (code desc &key default)
  "Macro for functionality related to HTTP status codes. Currently creates a condition and API handler.
'code' - the numerical status code (404, 500...)
'desc' - the description of the status code (\"Not found\", \"Internal server error\")
'default' - A format string for a default error message to be encoded in the JSON"

  (flet ((http-symbol (&rest args)
           (string-upcase (format nil "~{~A~}" args))))
    `(progn
       (defmethod handle-http-condition ((condition ,(intern (http-symbol code "-condition"))) handler request raw)
         (declare (ignorable raw))
         (with-slots (data) condition
           (with-chunked-stream-reply (handler request stream
                                       :code ,code :status ,desc
                                       :headers ((header-json-type)))
             (log-for (trace) "[Condition] ~A" condition)
             ,(if default
                `(json:encode-json `((:error . ,(format nil ,@default))) stream)
                `(json:encode-json `((:error . ,(format nil "~A" data))) stream)))))

       (define-condition ,(intern (http-symbol code "-condition")) (http-error-condition)
         ((data :initform ,desc
                :initarg :data
                :reader ,(intern (http-symbol code "-data"))))
         (:report (lambda (c s)
                    (format s ,(http-symbol code " Raised: ~A") (,(intern (http-symbol code "-data")) c)))))
       (export (find-symbol ,(http-symbol code "-condition") ':fdog-control) ':fdog-control))))

(def-http-code 400 "Bad request")
(def-http-code 404 "Not found" :default ("Endpoint ~A not found." (api-subpath request)))
(def-http-code 500 "Internal server error")

;; Utils
(defun api-subpath (request)
  (let* ((prefix "/api"))
    (ppcre:regex-replace (format nil "^~A" prefix)
                         (m2cl:request-path request) "")))

(defun header-json-type ()
  '("Content-Type" . "application/json"))

;; Router
(defgeneric api/endpoint (method sub-path handler request raw)
  (:documentation "Generic api endpoint. Things wishing to provide an API specialize on this method."))

(defgeneric api/endpoint-with-args (method sub-path rest handler request raw)
  (:documentation "Generic api endpoint with subpath for args. Things wishing to provide an API with a
variable URL component should specialize on this method.
The `sub-path' will not have a trailing slash, it will be on the `rest' side of the args."))

(defun api/router (handler request raw)
  (let ((method (intern (m2cl:request-header request :method) :keyword))
        (sub-path (api-subpath request)))

    (flet ((applicable-exact (sub-path)
             (let* ((sub-sym (intern sub-path :keyword))
                    (ef-meth (compute-applicable-methods #'api/endpoint (list method sub-sym handler request raw))))
               (values ef-meth
                       (if ef-meth
                           sub-sym
                           (unintern sub-sym)))))

           (next-sub-path (path)
             (multiple-value-bind (matchp parts) (ppcre:scan-to-strings "(^.+)(/.+)$" path)
               (when matchp
                 (apply #'values (coerce parts 'list)))))

           (applicable-sub (path rest)
             (log-for (trace) "Checking path: ~A with rest: ~A" path rest)
             (let* ((sub-sym (intern path :keyword))
                    (ef-meth (compute-applicable-methods #'api/endpoint-with-args
                                                         (list method sub-sym rest handler request raw))))
               (values ef-meth
                       (if ef-meth
                           sub-sym
                           (unintern sub-sym))))))

      (multiple-value-bind (exact exact-sym) (applicable-exact sub-path)
        (if exact
            (handler-case
                (api/endpoint method exact-sym handler request raw)
              (end-of-file () (handle-http-condition (make-instance '400-condition) handler request raw))
              (http-error-condition (condition) (handle-http-condition condition handler request raw)))

            (do* ((next-rest (multiple-value-list (next-sub-path sub-path))
                             (multiple-value-list (next-sub-path next)))
                  (next (car next-rest) (car next-rest))
                  (rest (cadr next-rest) (concatenate 'string (cadr next-rest) rest))
                  (found (and next (applicable-sub next rest))
                         (and next (applicable-sub next rest))))

                 ((or (not next) found)

                  (if found
                      (handler-case
                          (api/endpoint-with-args method (intern next :keyword) rest
                                                  handler request raw)
                          (end-of-file () (handle-http-condition (make-instance '400-condition) handler request raw))
                          (http-error-condition (condition) (handle-http-condition condition handler request raw)))

                      (handle-http-condition (make-instance '404-condition) handler request raw)))))))))
;; Endpoints
(defmethod api/endpoint ((m (eql :get)) (p (eql :/)) handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))

