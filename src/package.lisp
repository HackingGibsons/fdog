(defpackage #:afdog
  (:use #:cl)
  (:use #:log5)
  (:use :trivial-gray-streams)
  (:export :start-logging
           :stop-logging
           :make-local-sock
           :get-local-address
           :read-message
           :parse-message
           :*git-revision*
           :version-string
           :*root*
           :run-program
           :process-hash
           :kill-everything)
  (:export :*socket-linger*))

(in-package :afdog)

(defvar *root* (asdf:system-source-directory :afdog)
  "What is considered the root of the application.")

(defvar *git-revision* "HEAD")

(defparameter *socket-linger* 250
  "The linger period to use on all the zmq sockets.")

(defparameter *fdog-log-dir* "logs/")

(defparameter *default-root-path*
    (truename (probe-file (asdf:system-source-directory :afdog)))
      "Default for the root of the project: [Defaults to location of this file at load, if possible]")

(defparameter *root-path*
    *default-root-path*
      "The currently configured root path.")

(defparameter *socket-address* "ipc:///tmp/afdog-logging")


(defun version-string (&key (separator "/") (version t) (revision t))
  "Get a version string for people to read. The parameters
`version' and `revision' control the emission of the ASDF version and
the git revision, if available split by `separator' as a single string."
  (format nil (concatenate 'string "~@{~@[~A~]~^~:*~@[~*~@["separator"~]~]~}")
          (and version (asdf:component-version (asdf:find-system :afdog)))
          (and revision *git-revision*)))
