;;;; package.lisp
(defpackage #:fdog
  (:use #:cl)
  (:export :init
           :*default-root-path* :*default-server-path* :*default-server-database-path*
           :log-for :info :warn :error)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error)
  (:shadowing-import-from #:clsql))

(in-package :fdog)

;; Logging
(log5:start-sender 'default
                   (log5:stream-sender :location *error-output*)
                   :category-spec '(log5:dribble+)
                   :output-spec '(log5:time log5:category log5:message))


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


