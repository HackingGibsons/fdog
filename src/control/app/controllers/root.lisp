(in-package :fdog-control)

(defun root/respond (handler request raw)
  (flet ((string-responder (req)
           (format nil "Welcome to: ~A" (m2cl:request-path req))))
    (let ((full-responder (make-request-handler-string-responder handler #'string-responder))
          (path (m2cl:request-path request)))

      (funcall full-responder handler request raw)

      (log-for (trace) "Attempting to route for ~A" path))))

(defmacro with-handler-chunked-reply-chain ((handler &key (code 200) (status "OK")) &body body)
  (let ((g!handler (gensym "handler"))
        (g!header-fun (gensym "header-fun"))
        (g!start-c (gensym "start-c"))
        (g!stop-c (gensym "stop-c"))
        (g!code (gensym "code"))
        (g!status (gensym "status")))
    `(let ((,g!handler ,handler) (,g!code ,code) (,g!status ,status))
       (flet ((,g!header-fun (req)
                (declare (ignore req))
                `((:code . ,,g!code) (:status . ,,g!status))))
         (list
          (request-handler-make-chunked-responder/start ,g!handler #',g!header-fun)
          ,@body
          (request-handler-make-chunked-responder/stop ,g!handler))))))

(defun root/404 (handler request raw)
  (flet ((404-response (req)
           (format nil "~A Not Found" (m2cl:request-path req))))

    (request-handler-respond-with-chain handler request raw
      (with-handler-chunked-reply-chain (handler :code 404 :status "NOT FOUND")
        (request-handler-make-chunked-responder/chunk handler #'404-response)))))
