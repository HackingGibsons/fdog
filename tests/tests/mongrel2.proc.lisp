(in-package :fdog-tests)
(in-suite mongrel2/proc)

(test (can-have-running-server :fixture m2/with-running-server
                               :depends-on test-server-correct)
  (is (mongrel2-server-running-p server)
      "You cannot have a running server when you ask for one. [You should be able to] :("))

(test (can-reach-test-static-content-over-HTTP :fixture m2/with-running-server
                                               :depends-on can-have-running-server)
  (let* ((url (format nil "http://~A:~A/static/"
                                               (mongrel2-server-default-host-name server)
                                               (mongrel2-server-port server)))
         response)
    (setf response (drakma:http-request url))
    (is (ppcre:scan "^FDOG/OK" response) ;; TODO: Maybe slurp this from disk yourself?
        "Did not get the expected responce.")))
