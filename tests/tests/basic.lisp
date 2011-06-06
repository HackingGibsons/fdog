(in-package :fdog-tests)

(def-test (can-find-test-db-and-connect :group database-basic-tests
                                        :fixtures database/connected)
    :true (fdog-models:connected-p))

(def-eval-test (configured-db-has-mimetypes :group database-basic-tests
                                            :fixtures (database/connected database/inited))
    (let ((mimes (clsql:select 'mongrel2-mimetype :flatp t :refresh t)))
      (assert-non-nil mimes)
      (assert-non-nil (> (length mimes) 800))))

(def-eval-test (valid-endpoints-are-defined :group database-basic-tests)
    (let ((endpoints '(("proxy" . mongrel2-proxy)
                       ("handler" . mongrel2-handler)
                       ("dir" . mongrel2-directory))))
      (mapc (lambda (endpoint)
              (assert-equal (fdog-models::endpoint-by-name (car endpoint)) (cdr endpoint)))
            endpoints)))

(def-eval-test (can-connect-and-disconnect :group database-basic-tests
                                           :fixtures (database/connected database/inited))
    (progn
      (disconnect)
      (assert-null (connected-p))
      (connect)
      (assert-non-nil (connected-p))
      (disconnect)
      (assert-null (connected-p))))

(def-test (can-reconnect :group database-basic-tests
                         :fixtures database/connected)
    :true (progn
            (reconnect)
            (connected-p)))
