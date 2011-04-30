(in-package :fdog-tests)

(def-suite fdog-models :description "fdog-models tests")
(in-suite fdog-models)

(test valid-endpoints-are-defined
      (let ((endpoints '(("proxy" . mongrel2-proxy)
                         ("handler" . mongrel2-handler)
                         ("dir" . mongrel2-directory))))
        (mapc (lambda (endpoint)
                (is (equal (fdog-models::endpoint-by-name (car endpoint)) (cdr endpoint)))) 
              endpoints)))

(setf fdog:*default-server-database-path* (make-pathname :name "test" :type "sqlite"))

(test can-connect-and-disconnect-to-db
  (disconnect) ; incase we're already connected
  (is (null (connected-p)))
  (connect)
  (is (connected-p))
  (disconnect)
  (is (null (connected-p))))

(test can-reconnect
  (reconnect)
  (is (connected-p)))
