(in-package :fdog-control)

(defparameter *api-version* 1)

;; Utils
(defun api-subpath (request)
  (let* ((prefix "/api"))
    (ppcre:regex-replace (format nil "^~A" prefix)
                         (m2cl:request-path request) "")))

(defun header-json-type ()
  '("Content-Type" . "application/json"))

;; Common
(defun api/404 (handler request raw)
  (declare (ignorable raw))
  (with-chunked-stream-reply (handler request stream
                              :code 404 :status "NOT FOUND"
                              :headers ((header-json-type)))
    (json:encode-json `((:error . ,(format nil "Endpoint ~A not found." (api-subpath request)))) stream)))

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
            (api/endpoint method exact-sym handler request raw)

            (do* ((next-rest (multiple-value-list (next-sub-path sub-path))
                             (multiple-value-list (next-sub-path next)))
                  (next (car next-rest) (car next-rest))
                  (rest (cadr next-rest) (concatenate 'string (cadr next-rest) rest))
                  (found (and next (applicable-sub next rest))
                         (and next (applicable-sub next rest))))

                 ((or (not next) found)

                  (if found
                      (progn
                        (log-for (trace) "Found(~A) for: (~A)~A Rest: ~A" found (type-of next) next rest)
                        ;; TODO: Run this in a handler-case to catch any bubbling 404
                        (api/endpoint-with-args method (intern next :keyword) rest
                                                handler request raw))
                      (api/404 handler request raw)))))))))
;; Endpoints
(defmethod api/endpoint ((m (eql :get)) (p (eql :/)) handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))

