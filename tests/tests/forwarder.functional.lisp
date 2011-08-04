(in-package :fdog-tests)
;; Helpers
(defmacro def-test+func ((name &key fixtures) &body body)
  "Utility to shorten the process of writing an fdog functional test"
  `(def-test (,name :group fdog-forwarder-functional-tests
                    :fixtures (fdog/functional ,@fixtures))
       ,@body))

(defun assert-response-200 (meta)
  (assert-equal 200 (getf meta :status-code)))


;; Tests
(def-test+func (can-hit-slash) :eval
  (multiple-value-bind (res meta)  (http->string "http://localhost:1337/")
    (assert-non-nil res)
    (assert-response-200 meta)))

(def-test+func (can-create-forwarder-and-query-for-it) :eval
  (multiple-value-bind (res meta)  (http->json "http://localhost:1337/api/forwarders/")
    (assert-null res)
    (assert-response-200 meta))

  (let ((req '((:name . "test") (:hosts . (("localhost" . "/test/"))))))
    (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/create/" :method :POST
                                                :content (json:encode-json-to-string req))
      (assert-non-nil res)
      (assert-response-200 meta)))

  (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/")
    (assert-response-200 meta)
    (assert-non-nil res)

    (assert-non-nil (assoc :push res :test #'string-equal))
    (assert-non-nil (assoc :sub res :test #'string-equal))))
