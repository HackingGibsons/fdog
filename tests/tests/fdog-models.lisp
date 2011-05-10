(in-package :fdog-tests)

(in-suite main)

(test (can-find-test-db-and-connect :fixture db/connected)
  (is (fdog-models:connected-p)))

(test (configured-db-has-mimetypes :fixture db/inited)
  (let ((mimes (clsql:select 'mongrel2-mimetype :flatp t :refresh t)))
    (is-false (and (null mimes) (length mimes)))
    (is (> (length mimes) 800) "Mime-type count is NOT OVER 800!!!!!!!!!!!")))

(test valid-endpoints-are-defined
      (let ((endpoints '(("proxy" . mongrel2-proxy)
                         ("handler" . mongrel2-handler)
                         ("dir" . mongrel2-directory))))
        (mapc (lambda (endpoint)
                (is (equal (fdog-models::endpoint-by-name (car endpoint)) (cdr endpoint))))
              endpoints)))

(test (can-connect-and-disconnect :fixture db/connected)
  (disconnect) ; incase we're already connected
  (is (null (connected-p)))
  (connect)
  (is (connected-p))
  (disconnect)
  (is (null (connected-p))))

(test (can-reconnect :fixture db/connected)
  (is (connected-p) "We should be connected before we try to reconnect")
  (reconnect)
  (is (connected-p)))
