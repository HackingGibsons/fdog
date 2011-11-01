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
