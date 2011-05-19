(in-package :fdog-control)

(defun root/respond (handler request raw)
  (declare (ignorable raw))
  (with-chunked-stream-reply (handler request stream)
    (write-string "I guess it worked." stream)
    (format stream "~% Yep, sure did.~%")))

(defun root/404 (handler request raw)
  (with-chunked-reply-chain-response (handler request raw
                                              :code 404 :status "NOT FOUND")
      (&chunk (format nil "~%~A Not Found~%" (m2cl:request-path request)))))
