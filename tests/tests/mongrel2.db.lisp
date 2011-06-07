(in-package :fdog-tests)

;; Helpers
(defmacro def-test+m2/db (name &body body)
  "Utility to shorten the process of writing a mongrel2 db test"
  `(def-test (,name :group mongrel2-database-tests
                    :fixtures (database/connected database/inited database/configured))
       ,@body))

;; Tests
(def-test+m2/db can-find-test-server
  :true server)

(def-test+m2/db only-have-one-server
  :forms-equal 1 (length (fdog-m2sh:servers :refresh t)))

(def-test+m2/db server-can-find-hosts
  :true (and (mongrel2-server-hosts server)
             (= (length (mongrel2-server-hosts server)) 1)))

(def-test+m2/db server-default-host-fetchable
  (:all :true
        (:predicate (lambda (host) (string= (mongrel2-host-matching host)
                                            +default-host+))))
  (mongrel2-server-default-host server))

(def-test+m2/db server-default-host-localhost-exists
  (:all :true
        (:predicate (lambda (host) (string= (mongrel2-host-matching host)
                                            +default-host+)))
        (:predicate (lambda (host) (string= (mongrel2-host-matching host)
                                            (mongrel2-server-default-host-name server)))))
  (mongrel2-server-default-host server))

(def-test+m2/db server-default-host-has-routes
  (:all :true
        (:predicate (lambda (routes) (< 0 (length routes)))))

  (mongrel2-host-routes
   (mongrel2-server-default-host server)))

(def-test+m2/db server-default-host-has-/static/-route
  (:all :true
        (:predicate (lambda (s-routes) (< 0 (length s-routes)))))
  (remove-if-not #'(lambda (route-path) (string= "/static/" route-path))
                 (mongrel2-host-routes (mongrel2-server-default-host server))
                 :key #'mongrel2-route-path))

(def-test+m2/db server-/static/-route-is-a-directory
  (:all :true
        (:predicate (lambda (route) (mongrel2-route-target route)))
        (:predicate (lambda (route) (typep (mongrel2-route-target route)
                                             'mongrel2-directory))))
  (find "/static/" (mongrel2-host-routes (mongrel2-server-default-host server))
        :key #'mongrel2-route-path :test #'string=))

(def-test+m2/db test-server-correct
  (:all :true
        (:predicate (lambda (server) (string= +server-name+ (mongrel2-server-name server))))
        (:predicate (lambda (server) (ppcre:scan "^127\." (mongrel2-server-addr server))))
        (:predicate (lambda (server) (= (mongrel2-server-port server) +server-port+))))
  server)
