(in-package :api-app)

(defcategory api-app)
(defgeneric api (handler request raw)
  (:documentation "The root of the API App.")
  (:method (handler request raw)
    "Route the request somewhere else in the application"
    (with-dispatch-on (m2cl:request-path request) &route
        (funcall &route handler request raw)

      (:404 :responder 'api/404))))

;; Generic handlers
(defun api/404 (handler req raw)
  (declare (ignorable raw))
  (log-for (trace api-app) "404 on request: ~S" req)
  (with-chunked-stream-reply (handler req stream
                              :code 404 :status "NOT FOUND"
                              :headers ((header-json-type)))
    (json:encode-json-plist `(:error "Not Found") stream)))
