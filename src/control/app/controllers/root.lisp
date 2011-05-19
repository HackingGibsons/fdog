(in-package :fdog-control)

(defun root/respond (handler request raw)
  (with-chunked-reply-chain-response (handler request raw)
    (progn
      (log-for (dribble) "This should work")
      (&chunk (format nil "Path: ~A~%" (m2cl:request-path request)))
      (log-for (dribble) "Despite the lack of proper return."))))


(defun root/404 (handler request raw)
  (with-chunked-reply-chain-response (handler request raw
                                              :code 404 :status "NOT FOUND")
      (&chunk (format nil "~%~A Not Found~%" (m2cl:request-path request)))))
