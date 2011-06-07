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

;; (test (can-reach-test-static-content-over-HTTP :fixture m2/with-running-server
;;                                                :depends-on can-have-running-server)
;;   (let* ((url (format nil "http://~A:~A/static/"
;;                       (mongrel2-server-default-host-name server)
;;                       (mongrel2-server-port server)))
;;          (response (or (http->string url) "")))

;;     (is (ppcre:scan "^FDOG/OK" response) ;; TODO: Maybe slurp this from disk yourself?
;;           "Did not get the expected responce.")))
