(in-package :fdog-control)

(defun root/respond (handler request raw)
  (flet ((string-responder (req)
           (format nil "Welcome to: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Attempting to route for ~A" path))))

(defun root/404 (handler request raw)
  (flet ((the-404 (r) (declare (ignore r)) "I am so fucking awesome"))
    (with-chunked-reply-chain-response (handler request raw
                                                :code 404 :status "NOT FOUND")
      (&chunk #'the-404)
      (&chunk (format nil "~%~A Not Found~%" (m2cl:request-path request))))))
