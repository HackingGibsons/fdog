(in-package :fdog-tests)
;(in-suite basic)

(def-test (can-find-test-db-and-connect :group database-basic-tests)
    :true (fdog-models:connected-p))

(def-eval-test (configured-db-has-mimetypes :group database-basic-tests
                                            :fixtures database/inited)
    (let ((mimes (clsql:select 'mongrel2-mimetype :flatp t :refresh t)))
      (assert-non-nil mimes)
      (assert-non-nil (> (length mimes) 800))))



;; (test valid-endpoints-are-defined
;;       (let ((endpoints '(("proxy" . mongrel2-proxy)
;;                          ("handler" . mongrel2-handler)
;;                          ("dir" . mongrel2-directory))))
;;         (mapc (lambda (endpoint)
;;                 (is (equal (fdog-models::endpoint-by-name (car endpoint)) (cdr endpoint))))
;;               endpoints)))

;; (test (can-connect-and-disconnect :fixture db/connected)
;;   (disconnect) ; incase we're already connected
;;   (is (null (connected-p)))
;;   (connect)
;;   (is (connected-p))
;;   (disconnect)
;;   (is (null (connected-p))))

;; (test (can-reconnect :fixture db/connected)
;;   (is (connected-p) "We should be connected before we try to reconnect")
;;   (reconnect)
;;   (is (connected-p)))
