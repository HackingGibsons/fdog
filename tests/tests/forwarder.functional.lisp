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

(def-test+func (can-queue-request-then-serve-it-to-handler)
  :eval (assert-forwarder-setup)

  (log-for (trace) "Sending request destined for quedom.")
  (let ((old-request-count (fdog-forwarder:request-count "test")))
    (multiple-value-bind (res meta)  (http->json "http://localhost:13374/test/awesome/")
      (assert-null res)
      (assert-equal (1+ old-request-count) (fdog-forwarder:request-count "test"))))
  (log-for (trace) "Requet sent.")

  (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/")
    (assert-response-200 meta)
    (assert-non-nil res)

    (assert-non-nil (assoc :push res :test #'string-equal))
    (assert-non-nil (assoc :sub res :test #'string-equal))

    (let ((push (cdr (assoc :push res :test #'string-equal)))
          (sub (cdr (assoc :sub res :test #'string-equal))))

      (log-for (trace) "Endpoints: Push: ~A Sub: ~A" push sub)

      (flet ((s2us (s) (round (* s 1000000))))
        (m2cl:with-handler (handler "test" push sub)
          (multiple-value-bind (req raw) (m2cl:handler-receive handler :timeout (s2us 5))
            (redis:with-named-connection (redis :host fdog-forwarder:*redis-host*
                                                :port fdog-forwarder:*redis-port*)
            (assert-null (zerop (length raw)))
            (assert-non-nil (string-equal "/awesome/" (m2cl:request-path req)))
            (let ((old-response-count (fdog-forwarder:response-count "test")))
              (m2cl:handler-send-http handler "awesome!" :request req)
              (assert-non-nil (redis:lred-keys redis (format nil "~A*" fdog-forwarder:*response-prefix*)))
              (sleep 3) ;; TODO: the laziest test (race condition from response and actually hitting redis w/counter)
              (assert-equal (1+ old-response-count) (fdog-forwarder:response-count "test"))))))))))

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

      (assert-non-nil (assoc :push res :test #'string-equal))
      (assert-non-nil (assoc :sub res :test #'string-equal))

      (let ((push (cdr (assoc :push res :test #'string-equal)))
            (sub (cdr (assoc :sub res :test #'string-equal))))
        (flet ((s2us (s) (round (* s 1000000))))

          (m2cl:with-handler (handler "test" push sub)
            (multiple-value-bind (req raw) (m2cl:handler-receive handler :timeout (s2us 1))
              (assert-null (zerop (length raw)))
              (assert-non-nil (string-equal "/" (m2cl:request-path req))))))))

    (multiple-value-bind (res meta) (http->json "http://localhost:1337/api/forwarders/test/aliases/post-only/")
      (assert-response-200 meta)
      (assert-non-nil res)

      (assert-non-nil (assoc :push res :test #'string-equal))
      (assert-non-nil (assoc :sub res :test #'string-equal))

      (let ((push (cdr (assoc :push res :test #'string-equal)))
            (sub (cdr (assoc :sub res :test #'string-equal))))
        (flet ((s2us (s) (round (* s 1000000))))

          (m2cl:with-handler (handler "test" push sub)
            (multiple-value-bind (req raw) (m2cl:handler-receive handler :timeout (s2us 1))
              (assert-null (zerop (length raw)))
              (assert-non-nil (string-equal "/" (m2cl:request-path req)))))))))
