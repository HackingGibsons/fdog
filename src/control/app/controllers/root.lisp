(in-package :fdog-control)

(defun root/respond (handler request raw)
  (with-chunked-reply-chain-response (handler request raw)
    (&chunk (format nil "Path: ~A~%" (m2cl:request-path request)))))


(defun root/404 (handler request raw)
  (with-chunked-reply-chain-response (handler request raw
                                              :code 404 :status "NOT FOUND")
      (&chunk (format nil "~%~A Not Found~%" (m2cl:request-path request)))))
