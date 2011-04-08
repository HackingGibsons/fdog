;;;; fdog.lisp

(in-package #:fdog)

(defparameter *default-root*
  (if *load-pathname*
      (make-pathname :directory (pathname-directory *load-pathname*))
      (make-pathname :directory '(:relative ".")))
  "Default for the root of the project: [Defaults to location of this file at load, if possible]")

(defparameter *default-server-path* 
  (make-pathname :directory '(:relative "server"))
  "Default for the root of the server")

(defparameter *default-server-database*
  (make-pathname :name "config" :type "sqlite")
  "The default name of Mongrel2's database file")

;;; Init
(defun init (&key (root *default-root*) (server *default-server-path*) (database *default-server-database*))
  "Initialization function for fdog
Should find and assert the correctness of the project root, server dir, and then connect to the server database"
  (format t "I HAVE INITIALIZED LIKE A BAWS: ~A~%" (list root server database)))


;; (defvar *mongrel2-database*
;;   (clsql:connect `(,*local-mongrel2-database*) :database-type :sqlite3)
;;   "Database connection to the mongrel2 database")

