(in-package :fdog-tests)
;; Wrappers
(defmacro def-test+func ((name &key fixtures) &body body)
  "Utility to shorten the process of writing an fdog functional test"
  `(def-test (,name :group fdog-forwarder-functional-tests
                    :fixtures (fdog/functional ,@fixtures))
       ,@body))

;; Helpers
(defmacro assert-forwarder-setup ()
  "Utility to shorten the process of writing an fdog functional test"
  `(let ((req '((:name . "test") (:hosts . (("localhost" . "/test/"))))))
     (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/create/" :method :POST
                                                 :content (json:encode-json-to-string req))
       (assert-non-nil res)
       (assert-response-200 meta))))

(defmacro def-assert-response (code)
  `(defun ,(intern (string-upcase (format nil "assert-response-~A" code))) (meta)
     (assert-equal ,code (getf meta :status-code))))

(def-assert-response 200)
(def-assert-response 400)
(def-assert-response 404)

;; Tests
(def-test+func (can-hit-slash) :eval
  (multiple-value-bind (res meta)  (http->string "http://localhost:1337/")
    (assert-non-nil res)
    (assert-response-200 meta)))

(def-test+func (can-get-404) :eval
  (multiple-value-bind (res meta) (http->string "http://localhost:1337/forwarders/test/")
    (assert-non-nil res)
    (assert-response-404 meta)))

(def-test+func (can-get-400) :eval
  (multiple-value-bind (res meta)  (http->json "http://localhost:1337/api/forwarders/")
    (assert-null res)
    (assert-response-200 meta))

  (let ((req '((:name . "test"))))
    (multiple-value-bind (res meta) (http->string "http://localhost:1337/api/forwarders/create/" :method :POST
                                                  :content (json:encode-json-to-string req))
      (assert-non-nil res)
      (assert-response-400 meta))))

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

    (assert-non-nil (assoc :push res :test #'equal))
    (assert-non-nil (assoc :sub res :test #'equal))))

(def-test+func (can-queue-request-then-serve-it-to-handler)
  :eval (assert-forwarder-setup)

  (log-for (trace) "Sending request destined for quedom.")
  (let ((old-request-count (fdog-forwarder:request-count "test")))
    (multiple-value-bind (res meta)  (http->json "http://localhost:13374/test/awesome/")
      (assert-null res :format "Response not emptry")
      (assert-equal (1+ old-request-count) (fdog-forwarder:request-count "test") :format "Request count did not increment")))
  (log-for (trace) "Requet sent.")

  (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/")
    (assert-response-200 meta)
    (assert-non-nil res :format "Response is nil")

    (assert-non-nil (assoc :push res :test #'equal) :format "Response did not contain push")
    (assert-non-nil (assoc :sub res :test #'equal) :format "Response did not contain sub")

    (let ((push (cdr (assoc :push res :test #'equal)))
          (sub (cdr (assoc :sub res :test #'equal))))

      (log-for (trace) "Endpoints: Push: ~A Sub: ~A" push sub)

      (flet ((s2us (s) (round (* s 1000000))))
        (m2cl:with-handler (handler "test" push sub)
          (multiple-value-bind (req raw) (m2cl:handler-receive handler :timeout (s2us 5))
            (redis:with-named-connection (redis :host fdog-forwarder:*redis-host*
                                                :port fdog-forwarder:*redis-port*)
              (assert-null (zerop (length raw)) :format "Mongrel response not empty")
              (assert-non-nil (equal "/awesome/" (m2cl:request-path req)) :format "Path did not contain /awesome/")
              (let ((old-response-count (fdog-forwarder:response-count "test")))
                (m2cl:handler-send-http handler "awesome!" :request req)
                (sleep 3) ;; TODO: the laziest test (race condition from response and actually hitting redis)
                (assert-non-nil (redis:lred-keys redis (format nil "~A*" fdog-forwarder:*response-prefix*)) :format "Could not find response key in redis")
                (assert-equal (1+ old-response-count) (fdog-forwarder:response-count "test") :format "Response count did not increment")))))))))

(def-test+func (test-alias-function-and-routing)
    :eval (assert-forwarder-setup)

    (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/aliases/")
      (assert-null res)
      (assert-response-200 meta))

    (let ((req '((:name . "post-only") (method . "POST") (match . ".*"))))
      (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/aliases/create/" :method :POST
                                                  :content (json:encode-json-to-string req))
        (assert-non-nil res)
        (assert-response-200 meta)))

    (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/aliases/")
      (assert-non-nil res)
      (assert-response-200 meta)
      (assert-non-nil (find "post-only" res :test #'string=)))

    ;; Ensure that nothing that used to work broke.
    (log-for (trace) "Sending request destined for quedom to pick up.")
    (multiple-value-bind (res meta)  (http->json "http://localhost:13374/test/")
      (assert-null res))
    (log-for (trace) "Requet sent.")

    (log-for (trace) "Sending POST request destined for quedom to pick up.")
    (multiple-value-bind (res meta)  (http->json "http://localhost:13374/test/" :method :POST
                                                 :content "Hello world")
      (assert-null res))
    (log-for (trace) "POST Requet sent.")

    (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/")
      (assert-response-200 meta)
      (assert-non-nil res)

      (assert-non-nil (assoc :push res :test #'equal))
      (assert-non-nil (assoc :sub res :test #'equal))

      (let ((push (cdr (assoc :push res :test #'equal)))
            (sub (cdr (assoc :sub res :test #'equal))))
        (flet ((s2us (s) (round (* s 1000000))))

          (m2cl:with-handler (handler "test" push sub)
            (multiple-value-bind (req raw) (m2cl:handler-receive handler :timeout (s2us 1))
              (assert-null (zerop (length raw)))
              (assert-non-nil (equal "/" (m2cl:request-path req))))))))

    (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/aliases/post-only/")
      (assert-response-200 meta)
      (assert-non-nil res)

      (assert-non-nil (assoc :push res :test #'equal))
      (assert-non-nil (assoc :sub res :test #'equal))

      (let ((push (cdr (assoc :push res :test #'equal)))
            (sub (cdr (assoc :sub res :test #'equal))))
        (flet ((s2us (s) (round (* s 1000000))))

          (m2cl:with-handler (handler "test" push sub)
            (multiple-value-bind (req raw) (m2cl:handler-receive handler :timeout (s2us 1))
              (assert-null (zerop (length raw)))
              (assert-non-nil (equal "/" (m2cl:request-path req)))))))))
