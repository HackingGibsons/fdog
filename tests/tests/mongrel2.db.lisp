(in-package :fdog-tests)

(defmacro def-test+m2/db (name &body body)
  `(def-test (,name :group mongrel2-database-tests
                    :fixtures (database/connected database/inited database/configured))
       ,@body))

(def-test (can-find-test-server :group mongrel2-database-tests
                                :fixtures (database/connected database/inited database/configured))
    :true server)

(def-test (only-have-one-server :group mongrel2-database-tests
                                :fixtures (database/connected database/inited database/configured))
    :forms-equal 1 (length (fdog-m2sh:servers :refresh t)))

(def-test (server-can-find-hosts :group mongrel2-database-tests
                                 :fixtures (database/connected database/inited database/configured))
    :true (and (mongrel2-server-hosts server)
               (= (length (mongrel2-server-hosts server)) 1)))

(def-test+m2/db server-default-host-fetchable
  (:all :true
        (:predicate (lambda (host) (string= (mongrel2-host-matching host)
                                            +default-host+))))
  (mongrel2-server-default-host server))

;; (in-suite mongrel2/db)

;; (test (server-host-localhost-exists :fixture m2/with-server
;;                                     :depends-on server-can-find-hosts)
;;   (let ((host (car (mongrel2-server-hosts server))))
;;     (is (and (equal (mongrel2-host-matching host) +default-host+)
;;              (equal (mongrel2-host-matching host) (mongrel2-server-default-host-name server)))
;;         (format nil "The default host should be ~A and should exist" +default-host+))))

;; (test (server-default-host-has-routes :fixture m2/with-server
;;                                       :depends-on (and server-host-localhost-exists
;;                                                        server-default-host-fetchable))
;;   (let* ((host (mongrel2-server-default-host server))
;;          (routes (mongrel2-host-routes host)))
;;     (is (< 0 (length routes))
;;         "The default host needs to have routes")))

;; (test (server-default-host-has-/static/-route :fixture m2/with-server
;;                                               :depends-on server-default-host-has-routes)
;;   (let* ((routes (mongrel2-host-routes (mongrel2-server-default-host server)))
;;          (/static/-route (car (remove-if-not #'(lambda (r) (equal (mongrel2-route-path r) "/static/"))
;;                                              routes))))
;;     (is (< 0 (length routes))
;;         "I should have routes at this point. This violates dependency!")

;;     (is-false (null /static/-route)
;;               "One of the routes should be /static/")))

;; (test (server-/static/-route-is-a-directory :fixture m2/with-server+default-host
;;                                             :depends-on server-default-host-has-/static/-route)
;;   (let* ((route (car (mongrel2-host-routes default-host :path "/static/")))
;;          (target (mongrel2-route-target route)))
;;     (is-true target
;;              "There should be a target of at least some kind attached to the static route.")
;;     (is (typep target 'mongrel2-directory)
;;         "The /static/ route should be pointing to a directory")))


;; (test (test-server-correct :fixture m2/with-server
;;                                       :depends-on (and can-find-test-server
;;                                                        server-default-host-fetchable
;;                                                        server-/static/-route-is-a-directory))
;;   (is-false (null server) "We should have a server when we use the server fixture")

;;   (is (string-equal (mongrel2-server-name server) +server-name+)
;;       "I have reason to suspect you're using the wrong server.")

;;   (is (ppcre:scan "^127\." (mongrel2-server-addr server))
;;       "The test server should only listen on localhost")

;;   (is (= (mongrel2-server-port server) +server-port+)
;;       "The test server needs to use the test port"))
