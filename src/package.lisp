;;;; package.lisp
(defpackage #:fdog
  (:use #:cl)
  (:export :init :start
           :get-local-address
           :make-fdog-server-db-pathname
           :*root-path* :*default-root-path* :*default-server-path* :*default-server-database-path*
           :*fdog-run-dirname*
           :*fdog-master-pidfile*
           :fdog-pidfile-path
           :probe-fdog-pidfile
           :probe-fdog-pid
           :fdog-running-p)
  (:shadowing-import-from :sb-posix
                          :syscall-error
                          :kill)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error)
  (:shadowing-import-from #:clsql))

(in-package :fdog)

;; Logging
(log5:defoutput human-time (multiple-value-bind (second minute hour date month year)
                               (decode-universal-time (get-universal-time))
                             (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

(defun start-default-logging ()
  (log5:start-sender 'default
                     (log5:stream-sender :location *error-output*)
                     :category-spec '(log5:dribble+)
                     :output-spec '(human-time log5:category log5:message)))
(start-default-logging)


;; Parameters of project-wide relevance
(defparameter *default-root-path*
  (truename (probe-file (asdf:system-relative-pathname :fdog ".")))
  "Default for the root of the project: [Defaults to location of this file at load, if possible]")

(defparameter *root-path*
  *default-root-path*
  "The currently configured root path.")

(defparameter *default-server-path*
  (make-pathname :directory '(:relative "server"))
  "Default for the root of the server")

(defparameter *default-server-database-path*
  (make-pathname :name "config" :type "sqlite")
  "The default name of Mongrel2's database file")


(defparameter *fdog-run-dirname* "run/"
  "Directory containing the fdog run information such as PID files and run locks")

(defparameter *fdog-master-pidfile* "fdog.master.pid"
  "The name of the pid file to store in the run directory for the master instance")
