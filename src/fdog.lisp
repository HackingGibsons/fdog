;;;; fdog.lisp

(in-package #:fdog)

(defun make-fdog-server-db-pathname (&key (root *default-root-path*)
                                          (server *default-server-path*)
                                          (database *default-server-database-path*))
  (reduce #'merge-pathnames (list database server root)))

;;; Init
(defun init (&key (root *default-root-path*)
                  (server *default-server-path*)
                  (database *default-server-database-path*))
  "Initialization function for fdog
Should find and assert the correctness of the project root, server dir, and then connect to the server database"
  (setf *root-path* root)
  (fdog-models:connect (make-fdog-server-db-pathname :root root :server server :database database)))
