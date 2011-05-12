(in-package :fdog-tests)
(in-suite mongrel2/db)

(test (can-find-test-server :fixture db/configured)
  (let ((server (clsql:select 'mongrel2-server :flatp t :refresh t)))
    (is-false (null server))
    (is (= (length server) 1) "We really only should have the one test server")))

(test (server-can-find-hosts :fixture m2/with-server
                             :depends-on can-find-test-server)
  (let ((hosts (mongrel2-server-hosts server)))
    (is (= (length hosts) 1)
        "The server should have one host")))

(test (server-default-host-fetchable :fixture m2/with-server
                                     :depends-on server-can-find-hosts)
  (let ((host (mongrel2-server-default-host server)))
    (is-false (null host)
              "I should be able to fetch the default host")
    (is (equal (mongrel2-host-matching host) +default-host+)
        "The default host should match the defined default host")))

(test (server-host-localhost-exists :fixture m2/with-server
                                    :depends-on server-can-find-hosts)
  (let ((host (car (mongrel2-server-hosts server))))
    (is (and (equal (mongrel2-host-matching host) +default-host+)
             (equal (mongrel2-host-matching host) (mongrel2-server-default-host-name server)))
        (format nil "The default host should be ~A and should exist" +default-host+))))

(test (server-default-host-has-routes :fixture m2/with-server
                                      :depends-on (and server-host-localhost-exists
                                                       server-default-host-fetchable))
  (let* ((host (mongrel2-server-default-host server))
         (routes (mongrel2-host-routes host)))
    (is (< 0 (length routes))
        "The default host needs to have routes")))

(test (server-default-host-has-/static/-route :fixture m2/with-server
                                              :depends-on server-default-host-has-routes)
  (let* ((routes (mongrel2-host-routes (mongrel2-server-default-host server)))
         (/static/-route (car (remove-if-not #'(lambda (r) (equal (mongrel2-route-path r) "/static/"))
                                             routes))))
    (is (< 0 (length routes))
        "I should have routes at this point. This violates dependency!")

    (is-false (null /static/-route)
              "One of the routes should be /static/")))

(test (server-/static/-route-is-a-directory :fixture m2/with-server+default-host
                                            :depends-on server-default-host-has-/static/-route)
  (let* ((route (car (mongrel2-host-routes default-host :path "/static/")))
         (target (mongrel2-route-target route)))
    (is-true target
             "There should be a target of at least some kind attached to the static route.")
    (is (typep target 'mongrel2-directory)
        "The /static/ route should be pointing to a directory")))


(test (test-server-correct :fixture m2/with-server
                                      :depends-on (and can-find-test-server
                                                       server-default-host-fetchable))
  (is-false (null server) "We should have a server when we use the server fixture")

  (is (string-equal (mongrel2-server-name server) +server-name+)
      "I have reason to suspect you're using the wrong server.")

  (is (ppcre:scan "^127\." (mongrel2-server-addr server))
      "The test server should only listen on localhost")

  (is (= (mongrel2-server-port server) +server-port+)
      "The test server needs to use the test port"))
