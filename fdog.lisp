;;;; fdog.lisp

(in-package #:fdog)

;;; Init
(defun init (&key (root *default-root*) (server *default-server-path*) (database *default-server-database*))
  "Initialization function for fdog
Should find and assert the correctness of the project root, server dir, and then connect to the server database"
  (format t "I HAVE INITIALIZED LIKE A BAWS: ~A~%" (list root server database)))

