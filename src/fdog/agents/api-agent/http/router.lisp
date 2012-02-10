(in-package :http-dog)

(defcategory router)
(defmacro with-dispatch-on (route binding matched-form &body rules)
  (log-for (router trace) "Dispatching on ~A" route)
  (let (exact regex errors)
    (dolist (rule rules)
      (log-for (router trace) "Considering: ~A" rule)
      (destructuring-bind (type &rest options) rule
        (case type
          (:exact (log-for (router trace) "Exact route: ~A" options)
                  (push options exact))
          (:regex (log-for (router trace) "Regex route: ~A" options)
                  (push options regex))
          (otherwise (log-for (router trace) "Error: ~A" rule)
                     (push rule errors)))))
    (let ((g!route (gensym "route"))
          (g!exact (gensym "exact"))
          (g!regex (gensym "regex"))
          (g!error (gensym "error"))
          (g!match (gensym "match")))
      (log-for (router trace) "Exacts: ~A" exact)

      `(let* ((,g!route ,route) (,g!exact ',exact) (,g!regex ',regex) (,g!error ',errors)
              (,g!match (or (dolist (e-route ,g!exact)
                              (log-for (router dribble) "=> Current Exact test: ~A" e-route)
                              (destructuring-bind (path &rest options) e-route
                                (log-for (router dribble) "Checking exact route: ~A" path)
                                (when (string= path ,g!route)
                                  (log-for (router dribble) "Matched exact route: ~A => ~A" path options)
                                  (return options))))

                            (dolist (r-route ,g!regex)
                              (log-for (router dribble) "=> Current Regex test: ~A" r-route)
                              (destructuring-bind (path &rest options) r-route
                                (log-for (router dribble) "Checking regex route: ~A" path)
                                (when (ppcre:scan path ,g!route)
                                  (log-for (router dribble) "Matched regex route: ~A => ~A" path options)
                                  (return options))))

                            (cdr (or (find :default ,g!error :key #'car)
                                     (find :404 ,g!error :key #'car))))))
         (log-for (router dribble) "Matched route: ~A" ,g!match)
         (when ,g!match
           (let ((,binding (eval (getf ,g!match :responder))))
             ,matched-form))))))

;; Router utils
(defgeneric api/endpoint (method sub-path agent organ handler request raw)
  (:documentation "Generic api endpoint. Things wishing to provide an API specialize on this method."))

(defgeneric api/endpoint-with-args (method sub-path rest agent organ handler request raw)
  (:documentation "Generic api endpoint with subpath for args. Things wishing to provide an API with a
variable URL component should specialize on this method.
The `sub-path' will not have a trailing slash, it will be on the `rest' side of the args."))


(defun api/router (handler request &key)
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
