(defpackage #:afdog
  (:use #:cl)
  (:use #:log5)
  (:use :trivial-gray-streams)
  (:export :start-logging
           :stop-logging
           :make-local-sock
           :get-local-address
           :read-message
           :parse-message)
  (:export :*socket-linger*))

(in-package :afdog)

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
