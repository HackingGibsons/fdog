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
(defun api/router (handler request raw)
  (with-dispatch-on (api-subpath request) &route
    (funcall &route handler request raw)

    ;; TODO: Should return version or call a generic method
    ;;       with a subpath
    (:default :responder 'api/root)

    ;; TODO: Should not be in the routes, but be in the root responder
    ;;       and should fire when no generic method can be found
    (:404 :responder 'api/404)))

;; Endpoints
(defun api/root (handler request raw)
  (declare (ignorable raw))

  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))
