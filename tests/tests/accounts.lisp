(in-package :afdog-tests)

;; Accounts tests
;; No API key - expect 401
;; Incorrect API key - expect 401
;; Correct API key - expect "correct" behavior (whatever that is)

;; later - test that a correct API key exists in redis and a timeout
;; exists

(def-test (valid-api-key :group accounts-tests)
    (:eql 200)
  (let ((req '((:api--key . "valid-key") (:service . "test-api"))))
    (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api_clients/validate" *accounts-port*) :method :POST :content (json:encode-json-to-string req))
      (getf meta :status-code))))

(def-test (invalid-api-key :group accounts-tests)
    (:eql 401)
  (let ((req '((:api--key . "invalid-key") (:service . "test-api"))))
    (multiple-value-bind (res meta) (http->json (format nil "http://localhost:~A/api_clients/validate" *accounts-port*) :method :POST :content (json:encode-json-to-string req))
      (getf meta :status-code))))
