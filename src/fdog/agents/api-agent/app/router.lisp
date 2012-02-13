(in-package :api-app)

;; Router utils
(defgeneric api/endpoint (method sub-path agent organ handler request raw)
  (:documentation "Generic api endpoint. Things wishing to provide an API specialize on this method."))

(defgeneric api/endpoint-with-args (method sub-path rest agent organ handler request raw)
  (:documentation "Generic api endpoint with subpath for args. Things wishing to provide an API with a
variable URL component should specialize on this method.
The `sub-path' will not have a trailing slash, it will be on the `rest' side of the args."))

(defun api/router (handler request &key agent organ raw)
  (log-for (trace) "REMOVEME entered router")
  (log-for (trace) "REMOVEME AGENT ~A" agent)
  (let ((method (intern (m2cl:request-header request :method) :keyword))
        (sub-path (api-subpath request)))

    (flet ((applicable-exact (sub-path)
             (let* ((sub-sym (intern sub-path :keyword))
                    (ef-meth (compute-applicable-methods #'api/endpoint (list method sub-sym agent organ handler request raw))))
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
                                                         (list method sub-sym rest agent organ handler request raw))))
               (values ef-meth
                       (if ef-meth
                           sub-sym
                           (unintern sub-sym))))))

      (log-for (trace) "REMOVEME Method ~A sub-path ~A" method sub-path)
      (multiple-value-bind (exact exact-sym) (applicable-exact sub-path)
        (if exact
            (handler-case
                (api/endpoint method exact-sym agent organ handler request raw)
              (end-of-file () (handle-http-condition (make-instance '400-condition) agent organ handler request raw))
              (http-error-condition (condition) (handle-http-condition condition agent organ handler request raw)))

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
                                                  agent organ handler request raw)
                        (end-of-file () (handle-http-condition (make-instance '400-condition) agent organ handler request raw))
                        (http-error-condition (condition) (handle-http-condition condition agent organ handler request raw)))

                      (handle-http-condition (make-instance '404-condition) agent organ handler request raw)))))))))
