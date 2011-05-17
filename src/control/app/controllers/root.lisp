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
