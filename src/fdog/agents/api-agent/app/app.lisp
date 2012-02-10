(in-package :api-app)

(defvar *name* "afdog-api" "Name of the application")
(defvar *description* "AFdog network control API" "Description of the application")
(defvar *version* "0.0.0" "Application Version")
(defparameter *api-version* 2)

(defcategory api-app)
(defgeneric api (agent organ handler request raw)
  (:documentation "The root of the API App.")
  (:method (agent organ handler request raw)
    "Route the request somewhere else in the application"
    (with-dispatch-on (m2cl:request-path request) &route
        (funcall &route handler request
                 :raw raw :agent agent :organ organ
                 :allow-other-keys t)

      (:regex "^/api/.*" :responder 'api/router)

      (:exact "/" :responder 'api/version)

      (:404 :responder 'api/404))))

;; Generic handlers
(defun api/version (handler req &key)
  (with-chunked-stream-reply (handler req stream
                              :headers ((header-json-type)))
    (json:encode-json-plist `(:name ,*name* :description ,*description* :version ,*version*)
                            stream)))

(defun api/404 (handler req &key)
  (log-for (trace api-app) "404 on request: ~S" req)
  (with-chunked-stream-reply (handler req stream
                              :code 404 :status "NOT FOUND"
                              :headers ((header-json-type)))
    (json:encode-json-plist `(:error "Not Found") stream)))

;; Endpoints
(defmethod api/endpoint ((m (eql :get)) (p (eql :/)) agent organ handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))
