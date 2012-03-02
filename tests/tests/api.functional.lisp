(in-package :afdog-tests)

(def-test (can-hit-root :group api-functional-tests)
    (:values
     (:eql :match)
     (:eql 200))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/" *control-port*))
    (values
     (when (and
            ;; private because only used here to compare, deal with it
            (string= (cdr (assoc :name res)) api-app::*name*)
            (string= (cdr (assoc :description res)) api-app::*description*)
            (string= (cdr (assoc :version res)) api-app::*version*))
       :match)
     (getf meta :status-code))))

(def-test (can-hit-api-root :group api-functional-tests)
    (:values
     (:eql :match)
     (:eql 200))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/" *control-port*))
    (values
     ;; private because only used here to compare, deal with it
     (when (equal (cdr (assoc :version res)) api-app::*api-version*)
       :match)
     (getf meta :status-code))))

(def-test (can-hit-404 :group api-functional-tests)
    (:eql 404)
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/404" *control-port*))
    ;; TODO check response?
    (getf meta :status-code)))

(def-test (can-hit-400 :group api-functional-tests)
    (:eql 400)
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*) :method :POST
                                              :content (json:encode-json-to-string nil))
    ;; TODO check response?
    (getf meta :status-code)))

(def-test (can-hit-500 :group api-functional-tests) (:eql :pending) nil)
(def-test (can-hit-504 :group api-functional-tests) (:eql :pending) nil)

(def-test (posting-to-a-get-url-returns-404 :group api-functional-tests)
    (:eql 404)
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/" *control-port*) :method :POST)
    ;; TODO check response?
    (getf meta :status-code)))
(def-test (getting-to-a-post-url-returns-404 :group api-functional-tests)
    (:eql 404)
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*) :method :GET)
    ;; TODO check response?
    (getf meta :status-code)))

(def-test (can-create-forwarder :group api-functional-tests)
    (:values
     (:eql :match)
     (:eql 200))
  (let* ((forwarder-name "create")
         (host "api.example.com")
         (route-name "default")
         (route-path "/")
         (req `((:name . ,forwarder-name)
                (:hosts . ,(list host))
                (:routes . (((:name . ,route-name)
                             (:route . ,route-path)))))))
    (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                                                :method :POST
                                                :content (json:encode-json-to-string req))
      (values
       (when (and
              (cdr (assoc :success res))
              (string= (cdr (assoc :name res)) forwarder-name)
              (find host (cdr (assoc :hosts res)) :test #'string=)
              (find route-name (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :name route))))
              (find route-path (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :route route)))))
         :match)
       (getf meta :status-code)))))

(def-test (forwarder-info-formatted-correctly :group api-functional-tests) (:eql :pending) nil)
(def-test (cant-create-double-forwarder :group api-functional-tests) (:eql :pending) nil)
(def-test (can-delete-forwarder :group api-functional-tests) (:eql :pending) nil)
(def-test (cant-delete-nonexistent-forwarder :group api-functional-tests) (:eql :pending) nil)
(def-test (can-hit-health-check :group api-functional-tests) (:eql :pending) nil)

(def-test (forwarder-update-returns-403 :group api-functional-tests) (:eql :pending) nil)
(def-test (forwarder-metrics-returns-403 :group api-functional-tests) (:eql :pending) nil)
(def-test (aggregate-metrics-returns-403 :group api-functional-tests)
    (:eql 403)
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/metrics/" *control-port*))
    ;; TODO check response?
    (getf meta :status-code)))
