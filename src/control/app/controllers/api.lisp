(in-package :fdog-control)

(defparameter *api-version* 1)

;;; Macro for functionality related to HTTP status codes
;;; code - the numerical status code (404, 500...)
;;; desc - the description of the status code ("Not found", "Internal server error")
;;; echo - boolean whether to echo the status description in the JSON
;;; default - A format string for a default error message to be encoded in the JSON
;;; TODO: export symbol from fdog-control, shadow it in fdog-forwarder
;;; TODO: make these (intern (string-upcase ... function calls
(defmacro def-http-code (&key code desc echo default)
  `(progn (defun ,(intern (string-upcase (concatenate 'string "api/" (write-to-string code)))) (handler request raw &optional condition)
     (declare (ignorable raw))
     (with-chunked-stream-reply (handler request stream
                                :code ,code :status ,desc
                                :headers ((header-json-type)))
       (log-for (trace) "~A ~A Condition: ~A" ,code ,desc condition)
       ,(if echo
          `(json:encode-json `((:error . ,,desc)) stream)
          )
       (if condition
         (json:encode-json `((:error . ,(format nil "~A" condition))) stream)
         ,(if default
            `(json:encode-json `((:error . ,(format nil ,@default))) stream)
           )
         )))

  (define-condition ,(intern (string-upcase (concatenate 'string (write-to-string code) "-condition"))) ()
     ((data :initform ,desc
            :initarg :data
            :reader ,(intern (string-upcase (concatenate 'string (write-to-string code) "-data")))))
     (:report (lambda (c s)
                (format s ,(string-upcase (concatenate 'string (write-to-string code) " Raised: ~A")) (,(intern (string-upcase (concatenate 'string (write-to-string code) "-data"))) c)))))))

(def-http-code :code 400 :desc "Bad request" :echo t)
(def-http-code :code 404 :desc "Not found" :default ("Endpoint ~A not found." (api-subpath request)))
(def-http-code :code 500 :desc "Internal server error")

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
              (end-of-file () (api/400 handler request raw))
              (400-condition () (api/400 handler request raw))
              (404-condition () (api/404 handler request raw))
              (500-condition () (api/500 handler request raw)))

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
                        (end-of-file () (api/400 handler request raw))
                        (400-condition () (api/400 handler request raw))
                        (404-condition () (api/404 handler request raw))
                        (500-condition () (api/500 handler request raw)))

                      (api/404 handler request raw)))))))))
;; Endpoints
(defmethod api/endpoint ((m (eql :get)) (p (eql :/)) handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))

