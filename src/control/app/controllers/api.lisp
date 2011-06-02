(in-package :fdog-control)

(defparameter *api-version* 1)

;; Utils
(defun api-subpath (request)
  (let* ((prefix "/api"))
    (ppcre:regex-replace (format nil "^~A" prefix)
                         (m2cl:request-path request) "")))

(defun header-json-type ()
  '("Content-Type" . "application/json"))

;; Common
(defun api/404 (handler request raw)
  (declare (ignorable raw))
  (with-chunked-stream-reply (handler request stream
                              :code 404 :status "NOT FOUND"
                              :headers ((header-json-type)))
    (json:encode-json `((:error . ,(format nil "Endpoint ~A not found." (api-subpath request)))) stream)))

;; Router
(defgeneric api/endpoint (method sub-path handler request raw)
  (:documentation "Generic api endpoint. Things wishing to provide an API specialize on this method."))

(defgeneric api/endpoint-with-args (method sub-path rest handler request raw)
  (:documentation "Generic api endpoint with subpath for args. Things wishing to provide an API with a
variable URL component should specialize on this method."))

(defun api/router (handler request raw)
  (let ((method (intern (m2cl:request-header request :method) :keyword))
        (sub-path (api-subpath request)))

    (flet ((applicable-exact (sub-path)
             (let* ((sub-sym (intern sub-path :keyword))
                    (ef-meth (compute-applicable-methods #'api/endpoint (list method sub-sym handler request raw))))
               (values ef-meth
                       (if ef-meth
                           sub-sym
                           (unintern sub-sym))))))

      (multiple-value-bind (exact exact-sym) (applicable-exact sub-path)

        (if exact
            (api/endpoint method exact-sym handler request raw)
            (api/404 handler request raw))))))

;; Endpoints
(defmethod api/endpoint ((m (eql :get)) (p (eql :/)) handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))

