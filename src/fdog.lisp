;;;; fdog.lisp

(in-package #:fdog)

;;; Init
(defun init (&key (root *default-root-path*) (server *default-server-path*) (database *default-server-database-path*))
  "Initialization function for fdog
Should find and assert the correctness of the project root, server dir, and then connect to the server database"
  (fdog-models:connect (reduce #'merge-pathnames (list database server root))))
