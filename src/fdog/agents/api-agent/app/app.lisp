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

(defun api/404 (handler req &key agent organ raw)
  (log-for (trace api-app) "404 on request: ~S" req)
  (handle-http-condition (make-instance '404-condition) agent organ handler req raw))

(defmethod api/forwarder/404 ((agent api-agent) organ handler request forwarder rest)
  (error '404-condition
         ;; TODO doesn't return name - (name forwarder)
         :details (format nil "No resource for forwarder ~A matching ~A" forwarder rest)))

;; Endpoints
(defmethod api/endpoint ((m (eql :get)) (p (eql :/)) (agent api-agent) organ handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `((:version . ,*api-version*)) stream)))

(defmethod api/endpoint-with-args ((m (eql :get)) (p (eql :|/forwarders|)) rest (agent api-agent) organ handler request raw)
  (ppcre:register-groups-bind (name rest) ("^/?([^/]+)(/?.*$)" rest)
    (let ((forwarder (find-forwarder agent name)))
      (unless forwarder
        ;; TODO doesn't return name
        (error '404-condition :details (format nil "Forwarder ~A not found" name)))

      (with-dispatch-on rest &route
          (funcall &route agent organ handler request forwarder rest)

        (:exact "/metrics/" :responder 'api/forwarder/metrics)
        (:exact "/" :responder 'api/forwarder/root)
        (:404 :responder 'api/forwarder/404)))))

(defmethod api/endpoint-with-args ((m (eql :post)) (p (eql :|/forwarders|)) rest (agent api-agent) organ handler request raw)
  (ppcre:register-groups-bind (name rest) ("^/?([^/]+)(/?.*$)" rest)
    (let ((forwarder (find-forwarder agent name)))
      (unless forwarder
        ;; TODO doesn't return name
        (error '404-condition :details (format nil "Forwarder ~A not found" name)))

      (with-dispatch-on rest &route
          (funcall &route agent organ handler request forwarder rest)

        (:exact "/delete/" :responder 'api/forwarder/delete)
        (:exact "/create/" :responder 'api/forwarder/create)
        (:exact "/" :responder 'api/forwarder/update)
        (:404 :responder 'api/forwarder/404)))))

(defmethod api/endpoint ((m (eql :get)) (p (eql :|/forwarders/|)) (agent api-agent) organ handler request raw)
  "Retrieves information about all known forwarders."
  (error '403-condition :details "TODO forwarders root"))

(defmethod api/forwarder/root ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "TODO forwarder details"))

(defmethod api/forwarder/create ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "TODO forwarder create"))

(defmethod api/forwarder/delete ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "TODO forwarder delete"))

(defmethod api/forwarder/update ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "TODO forwarder update"))

(defmethod api/forwarder/metrics ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "TODO forwarder metrics"))
