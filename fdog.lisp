;;;; fdog.lisp

(in-package #:fdog)

(defvar *local-mongrel2-database-pathname* 
  (make-pathname :directory '(:relative "server")
                 :name "config" :type "sqlite")
  "Wild pathname used to find the local mongrel2 database")

(defvar *local-mongrel2-database* 
  (namestring (first (directory *local-mongrel2-database-pathname*)))
  "Proper pathname to the mongrel2 database")

(defvar *mongrel2-database* 
  (clsql:connect `(,*local-mongrel2-database*) :database-type :sqlite3)
  "Database connection to the mongrel2 database")

