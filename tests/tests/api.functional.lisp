(in-package :afdog-tests)

(def-test (can-hit-root :group api-functional-tests)
    (:values
     (:eql 200)
     (:eql :version-info-exists))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/" *control-port*))
    (values
     (getf meta :status-code)
     (when (and
            ;; private because only used here to compare, deal with it
            (string= (cdr (assoc :name res)) api-app::*name*)
            (string= (cdr (assoc :description res)) api-app::*description*)
            (string= (cdr (assoc :version res)) api-app::*version*))
       :version-info-exists))))

(def-test (can-hit-api-root :group api-functional-tests)
    (:values
     (:eql 200)
     (:eql :version-exists))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/" *control-port*))
    (values
     (getf meta :status-code)
     ;; private because only used here to compare, deal with it
     (when (equal (cdr (assoc :version res)) api-app::*api-version*)
       :version-exists))))

(def-test (can-hit-404 :group api-functional-tests)
    (:values
     (:eql 404)
     (:eql :not-found))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/404" *control-port*))
    (values
     (getf meta :status-code)
     (when (ppcre:scan "found" (cdr (assoc :details res)))
       :not-found))))

(def-test (can-hit-400 :group api-functional-tests)
    (:values
     (:eql 400)
     (:eql :params-missing))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*) :method :POST
                                              :content (json:encode-json-to-string nil))
    (values
     (getf meta :status-code)
     (when (ppcre:scan "missing or malformed" (cdr (assoc :details res)))
       :params-missing))))

(def-test (can-hit-504 :group api-functional-tests) (:eql :pending) nil)

(def-test (posting-to-a-get-url-returns-404 :group api-functional-tests)
    (:eql 404)
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/" *control-port*) :method :POST)
    (declare (ignorable res))
    (getf meta :status-code)))

(def-test (getting-to-a-post-url-returns-404 :group api-functional-tests)
    (:values
     (:eql 404)
     (:eql :not-found))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*) :method :GET)
    (values
     (getf meta :status-code)
     (when (ppcre:scan "found" (cdr (assoc :details res)))
       :not-found))))

(def-test (can-create-forwarder :group api-functional-tests)
    (:values
     (:eql 200)
     (:eql :forwarder-info-echoed))
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
       (getf meta :status-code)
       (when (and
              (cdr (assoc :success res))
              (string= (cdr (assoc :name res)) forwarder-name)
              (find host (cdr (assoc :hosts res)) :test #'string=)
              (find route-name (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :name route))))
              (find route-path (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :route route)))))
         :forwarder-info-echoed)))))

(def-test (forwarder-list-formatted-correctly :group api-functional-tests
  :setup (progn
           (let* ((forwarder-name "list")
                  (host "api.example.com")
                  (route-name "default")
                  (route-path "/list/")
                  (req `((:name . ,forwarder-name)
                         (:hosts . ,(list host))
                         (:routes . (((:name . ,route-name)
                                      (:route . ,route-path)))))))
             (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                         :method :POST
                         :content (json:encode-json-to-string req)))
           (wait-for-agent-message (forwarder-agent-uuid :timeout 3) (msg)
             (when (getf msg :filled)
               :forwarder-created))))
    (:values
     (:eql 200)
     (:eql :found))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/" *control-port*))
    (values
     (getf meta :status-code)
     (when (find "list" (cdr (assoc :forwarders res)) :test #'string=)
       :found))))

(def-test (forwarder-info-formatted-correctly :group api-functional-tests
  :setup (progn
           (let* ((forwarder-name "info")
                  (host "api.example.com")
                  (route-name "default")
                  (route-path "/info/")
                  (req `((:name . ,forwarder-name)
                         (:hosts . ,(list host))
                         (:routes . (((:name . ,route-name)
                                      (:route . ,route-path)))))))
             (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                         :method :POST
                         :content (json:encode-json-to-string req)))
           (wait-for-agent-message (forwarder-agent-uuid :timeout 3) (msg)
             (when (getf msg :filled)
               :forwarder-created))))
    (:values
     (:eql 200)
     (:eql :info-exists)
     (:eql :handlers-exist))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/info/" *control-port*))
    (values
     (getf meta :status-code)
     (when (and
            (string= (cdr (assoc :name res)) "info")
            (find "api.example.com" (cdr (assoc :hosts res)) :test #'string=)
            (find "default" (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :name route))))
            (find "/info/" (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :route route)))))
       :info-exists)
     (let ((route (find "default" (cdr (assoc :routes res)) :test #'string= :key #'(lambda (route) (cdr (assoc :name route))))))
       (when (and
              (assoc :push route)
              (assoc :sub route))
         :handlers-exist)))))

(def-test (cant-double-create-forwarder :group api-functional-tests
  :setup (progn
           (let* ((forwarder-name "double")
                  (host "api.example.com")
                  (route-name "default")
                  (route-path "/db/")
                  (req `((:name . ,forwarder-name)
                         (:hosts . ,(list host))
                         (:routes . (((:name . ,route-name)
                                      (:route . ,route-path)))))))
             (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                         :method :POST
                         :content (json:encode-json-to-string req)))
           (wait-for-agent-message (forwarder-agent-uuid :timeout 3) (msg)
             (when (getf msg :filled)
               :forwarder-created))))
    (:values
     (:eql 400)
     (:eql :already-exists))
  (let* ((forwarder-name "double")
         (host "api.example.com")
         (route-name "default")
         (route-path "/db/")
         (req `((:name . ,forwarder-name)
                (:hosts . ,(list host))
                (:routes . (((:name . ,route-name)
                             (:route . ,route-path)))))))
    (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                 :method :POST
                 :content (json:encode-json-to-string req))
      (values
       (getf meta :status-code)
       (when
           (ppcre:scan "already exists" (cdr (assoc :details res)))
         :already-exists)))))

(def-test (can-delete-forwarder :group api-functional-tests
  :setup (progn
           (let* ((forwarder-name "deleteme")
                  (host "api.example.com")
                  (route-name "default")
                  (route-path "/dm/")
                  (req `((:name . ,forwarder-name)
                         (:hosts . ,(list host))
                         (:routes . (((:name . ,route-name)
                                      (:route . ,route-path)))))))
             (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                         :method :POST
                         :content (json:encode-json-to-string req)))
           (wait-for-agent-message (forwarder-agent-uuid :timeout 3) (msg)
             (when (getf msg :filled)
               :forwarder-created))))
    (:values
     (:eql 200)
     (:eql :forwarder-deleted))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/deleteme/delete/" *control-port*)
                                              :method :POST)
    (values
     (getf meta :status-code)
     (when
         (cdr (assoc :success res))
       :forwarder-deleted))))


(def-test (cant-delete-nonexistent-forwarder :group api-functional-tests)
    (:values
     (:eql 404)
     (:eql :not-found))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/nonexistent/delete/" *control-port*)
                                                      :method :POST)
    (values
     (getf meta :status-code)
     (when (ppcre:scan "found" (cdr (assoc :details res)))
       :not-found))))

(def-test (can-hit-health-check :group api-functional-tests)
    (:values
     (:eql 200)
     (:eql :health-info-found))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/healthcheck/" *control-port*))
    (values
     (getf meta :status-code)
     (when
         (string= (cdr (assoc :state res)) "ok")
       :health-info-found))))

(def-test (forwarder-update-returns-403 :group api-functional-tests
  :setup (progn
           (let* ((forwarder-name "updateme")
                  (host "api.example.com")
                  (route-name "default")
                  (route-path "/u/")
                  (req `((:name . ,forwarder-name)
                         (:hosts . ,(list host))
                         (:routes . (((:name . ,route-name)
                                      (:route . ,route-path)))))))
             (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                         :method :POST
                         :content (json:encode-json-to-string req)))
           (wait-for-agent-message (forwarder-agent-uuid :timeout 3) (msg)
             (when (getf msg :filled)
               :forwarder-created))))
    (:values
     (:eql 403)
     (:eql :not-yet-implemented))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/updateme/update/" *control-port*)
                                              :method :POST)
    (values
     (getf meta :status-code)
     (when (ppcre:scan "implemented" (cdr (assoc :details res)))
       :not-yet-implemented))))

(def-test (forwarder-metrics-returns-403 :group api-functional-tests
  :setup (progn
           (let* ((forwarder-name "metrics")
                  (host "api.example.com")
                  (route-name "default")
                  (route-path "/metrics/")
                  (req `((:name . ,forwarder-name)
                         (:hosts . ,(list host))
                         (:routes . (((:name . ,route-name)
                                      (:route . ,route-path)))))))
             (http->json (format nil "http://localhost:~A/api/forwarders/create/" *control-port*)
                         :method :POST
                         :content (json:encode-json-to-string req)))
           (wait-for-agent-message (forwarder-agent-uuid :timeout 3) (msg)
             (when (getf msg :filled)
               :forwarder-created))))
    (:values
     (:eql 403)
     (:eql :not-yet-implemented))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/forwarders/metrics/metrics/" *control-port*))
    (values
     (getf meta :status-code)
     (when (ppcre:scan "implemented" (cdr (assoc :details res)))
       :not-yet-implemented))))

(def-test (aggregate-metrics-returns-403 :group api-functional-tests)
    (:values
     (:eql 403)
     (:eql :not-yet-implemented))
  (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api/metrics/" *control-port*))
    (values
     (getf meta :status-code)
     (when (ppcre:scan "implemented" (cdr (assoc :details res)))
       :not-yet-implemented))))
