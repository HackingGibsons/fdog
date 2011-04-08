;;;; package.lisp
(defpackage #:fdog
  (:use #:cl)
  (:export :init
           :*default-root-path* :*default-server-path* :*default-server-database-path*)
  (:shadowing-import-from #:clsql))

(in-package :fdog)


;; Parameters of project-wide relevance
(defparameter *default-root-path*
  (truename (make-pathname :directory '(:relative ".")))
  "Default for the root of the project: [Defaults to location of this file at load, if possible]")

(defparameter *default-server-path*
  (make-pathname :directory '(:relative "server"))
  "Default for the root of the server")

(defparameter *default-server-database-path*
  (make-pathname :name "config" :type "sqlite")
  "The default name of Mongrel2's database file")


