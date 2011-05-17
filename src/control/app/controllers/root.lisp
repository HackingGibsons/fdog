(in-package :fdog-control)

(defun root/respond (handler request raw)
  (flet ((string-responder (req)
           (format nil "Welcome to: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Attempting to route for ~A" path))))

(defmacro with-chunked-reply ((handler &key (code 200) (status "OK")) &body body)
  `(let ()
     :undef))

(defun root/404% (handler request raw)
  (flet ((404-response (req)
           (format nil "~A Not Found" (m2cl:request-path req))))
    (let ((404-responer (request-handler-make-chunked-responder/chunk handler 404-response)))
      (with-chunked-reply (handler :code 404 :status "NOT FOUND")
        (funcall 404-responer handler request raw)))))

(defun root/404 (handler request raw)
  (flet ((string-responder (req)
           (format nil "404 Not Found: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Served 404 for ~A" path))))
