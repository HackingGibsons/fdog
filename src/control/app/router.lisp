(in-package :fdog-control)

(defun root/respond (handler request raw)
  (flet ((string-responder (req)
           (format nil "Welcome to: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Attempting to route for ~A" path))))

(defun root/404 (handler request raw)
  (flet ((string-responder (req)
           (format nil "404 Not Found: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Served 404 for ~A" path))))

(defmacro dispatch-on (route &rest rules)
  (log-for (trace) "Dispatching on ~A" route)
  (let (exact regex errors)
    (dolist (rule rules)
      (log-for (trace) "Considering: ~A" rule)
      (destructuring-bind (type &rest options) rule
        (case type
          (:exact (log-for (trace) "Exact route: ~A" options)
                  (push options exact))
          (:regex (log-for (trace) "Regex route: ~A" options)
                  (push options regex))
          (otherwise (log-for (trace) "Error: ~A" rule)
                     (push rule errors)))))
    (let ((g!route (gensym "route"))
          (g!exact (gensym "exact"))
          (g!regex (gensym "regex"))
          (g!error (gensym "error"))
          (g!match (gensym "match")))
      `(let ()
         (log-for (trace) "Dispatch begin")

         (log-for (trace) "Dispatch end")))))

(defun root/router% (handler request raw)
  (dispatch-on (m2cl:request-path request)
               (:exact "/" :responder 'root/respond)

               (:404 :responder 'root/404)))

(defun root/router (handler request raw)
  (flet ((string-responder (req)
           (format nil "Welcome to: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Attempting to route for ~A" path))))

