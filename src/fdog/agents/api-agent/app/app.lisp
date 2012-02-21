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
        (error '404-condition :details (format nil "Forwarder ~A not found" name)))

      (with-dispatch-on rest &route
          (funcall &route agent organ handler request forwarder rest)

        (:exact "/delete/" :responder 'api/forwarder/delete)
        (:exact "/" :responder 'api/forwarder/update)
        (:404 :responder 'api/forwarder/404)))))

(defmethod api/endpoint ((m (eql :get)) (p (eql :|/forwarders/|)) (agent api-agent) organ handler request raw)
  "Retrieves information about all known forwarders."
  (with-chunked-stream-reply (handler request stream
                                      :headers ((header-json-type)))
    (let ((forwarder-list (mapcar #'forwarder-to-alist (forwarders agent))))
      (json:encode-json-alist forwarder-list stream))))

(defmethod api/forwarder/root ((agent api-agent) organ handler request forwarder rest)
  (with-chunked-stream-reply (handler request stream
                                      :headers ((header-json-type)))
    ;; We just want the forwarder information, not the full thing like
    ;; in /forwarders/ list
    (json:encode-json-alist (cdr (forwarder-to-alist forwarder)) stream)))

(defmethod api/endpoint ((m (eql :post)) (p (eql :|/forwarders/create/|)) (agent api-agent) organ handler request raw)
  (let* ((spec (decode-json-from-request (m2cl:request-body request)))
         (name (cdr (assoc :name spec)))
         (hosts (cdr (assoc :hosts spec)))
         (routes (cdr (assoc :routes spec)))
         (existing (find-forwarder agent name)))
    (unless (and spec name hosts routes)
      (error '400-condition :details "Parameters missing or malformed"))
    (if existing
        (error '400-condition :details (format nil "Forwarder ~A already exists" name))
        (send-message (find-organ agent :head) :command
                      `(:command :speak
                                 :say (:agent :need
                                              :need :forwarder
                                              :forwarder (:name ,name :hosts ,hosts :routes ,routes)))))

    (register-callback agent
                       (make-instance 'callback
                          :predicate #'(lambda (from type event)
                                         (and (eql from :filled) (string= name (getf (getf event :forwarder) :name))))
                          :callback #'(lambda ()
                                        (with-chunked-stream-reply (handler request stream
                                                                            :headers ((header-json-type)))
                                          (json:encode-json-alist `(("status" . "ok") ,@spec) stream)))
                          :timeout-callback #'(lambda ()
                                                (handle-http-condition (make-instance '504-condition) agent organ handler request raw))))))

(defmethod api/forwarder/delete ((agent api-agent) organ handler request forwarder raw)
  (let ((name (car forwarder)))
    (send-message (find-organ agent :head) :command
                  `(:command :speak
                             :say (:agent :need
                                          :need :remove-forwarders
                                          :remove-forwarders
                                          (:names (,name)))))
    (register-callback agent
                       (make-instance 'callback
                          :predicate #'(lambda (from type event)
                                         (and (eql from :filled) (string= name (car (getf (getf event :remove-forwarders) :names)))))
                          :callback #'(lambda ()
                                        (with-chunked-stream-reply (handler request stream
                                                                            :headers ((header-json-type)))
                                          (json:encode-json-alist `(("status" . "ok")) stream)))
                          :timeout-callback #'(lambda ()
                                                (handle-http-condition (make-instance '504-condition) agent organ handler request raw))))))

(defmethod api/forwarder/update ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "Not yet implemented"))

(defmethod api/forwarder/metrics ((agent api-agent) organ handler request forwarder rest)
  (error '403-condition :details "Not yet implemented"))
