(in-package :afdog-tests)

(def-test (can-hit-root :group api-functional-tests)
    (:values
     (:eql :match)
     (:eql 200))
  (multiple-value-bind (res meta) (http->string (format nil "http://localhost:~A/" *control-port*))))
(def-test (can-hit-api-root :group api-functional-tests) (:eql :pending))
(def-test (can-hit-404 :group api-functional-tests) (:eql :pending))
(def-test (can-hit-400 :group api-functional-tests) (:eql :pending))
(def-test (can-hit-500 :group api-functional-tests) (:eql :pending))
(def-test (posting-to-a-get-url-returns-404 :group api-functional-tests) (:eql :pending))
(def-test (getting-to-a-post-url-returns-404 :group api-functional-tests) (:eql :pending))

(def-test (can-create-forwarder :group api-functional-tests) (:eql :pending))
(def-test (forwarder-info-formatted-correctly :group api-functional-tests) (:eql :pending))
(def-test (cant-create-double-forwarder :group api-functional-tests) (:eql :pending))
(def-test (can-delete-forwarder :group api-functional-tests) (:eql :pending))
(def-test (cant-delete-nonexistent-forwarder :group api-functional-tests) (:eql :pending))
(def-test (can-hit-health-check :group api-functional-tests) (:eql :pending))

(def-test (forwarder-update-returns-403 :group api-functional-tests) (:eql :pending))
(def-test (forwarder-metrics-returns-403 :group api-functional-tests) (:eql :pending))
(def-test (aggregate-metrics-returns-403 :group api-functional-tests) (:eql :pending))
