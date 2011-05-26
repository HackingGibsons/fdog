(defpackage #:fdog-forwarder
  (:use #:cl)
  (:shadowing-import-from :fdog-control
                          :api/endpoint)
  (:shadowing-import-from :log5 :log-for)
  (:shadowing-import-from :clsql)

  (:shadowing-import-from :fdog-control
                          :fdog-interface)

  (:export :init-forwarders))

(in-package :fdog-forwarder)

(defvar *forwarders* ()
  "List of the loaded forwarders")

(defclass fdog-forwarding-interface (fdog-interface)
  ((upstream :initarg :upstream
             :accessor forwarder-upstream))
  (:documentation "An interface for forwarding requests to upstream 0mq endpoints."))

(defmethod init-forwarders ()
  "Search for, init and start all known forwarders"
  (log-for (dribble) "Initializing forwarders..")
  :undef)
