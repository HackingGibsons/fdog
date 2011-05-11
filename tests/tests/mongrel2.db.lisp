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

(test (server-host-localhost-exists :fixture m2/with-server
                                    :depends-on server-can-find-hosts)
  (let ((host (car (mongrel2-server-hosts server))))
    (is (and (equal (mongrel2-host-matching host) +default-host+)
             (equal (mongrel2-host-matching host) (mongrel2-server-default-host server)))
        (format nil "The default host should be ~A and should exist" +default-host+))))

(test (test-server-correct :fixture m2/with-server
                                      :depends-on (and can-find-test-server
                                                       server-can-find-hosts))
  (is-false (null server) "We should have a server when we use the server fixture")

  (is (string-equal (mongrel2-server-name server) +server-name+)
      "I have reason to suspect you're using the wrong server.")

  (is (ppcre:scan "^127\." (mongrel2-server-addr server))
      "The test server should only listen on localhost")

  (is (= (mongrel2-server-port server) +server-port+)
      "The test server needs to use the test port"))
