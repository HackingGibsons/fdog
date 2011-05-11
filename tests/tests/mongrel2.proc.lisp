(in-package :fdog-tests)
(in-suite mongrel2/proc)

(test (can-have-running-server :fixture m2/with-running-server
                               :depends-on test-server-correct)
  (is (mongrel2-server-running-p server)
      "You cannot have a running server when you ask for one. [You should be able to] :("))
