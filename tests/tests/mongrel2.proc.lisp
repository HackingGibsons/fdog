(in-package :fdog-tests)
;; Helpers
(defmacro def-test+m2/running (name &body body)
  "Utility to shorten the process of writing a mongrel2 db test"
  `(def-test (,name :group mongrel2-process-tests
                    :fixtures (database/connected database/inited database/configured mongrel2/running))
       ,@body))

;; Tests
(def-test+m2/running can-have-running-server
  :true (mongrel2-server-running-p server))

(def-test+m2/running can-reach-test-static-content-over-HTTP
  :eval
  (let* ((url (format nil "http://~A:~A/static/"
                      (mongrel2-server-default-host-name server)
                      (mongrel2-server-port server)))
         (response (or (http->string url) "")))
    (assert-non-nil (ppcre:scan "^FDOG/OK" response))))
