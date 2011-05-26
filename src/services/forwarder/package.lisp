(defpackage #:fdog-forwarder
  (:use #:cl)
  (:shadowing-import-from :fdog-control
                          :api/endpoint)
  (:shadowing-import-from :log5 :log-for)

  (:shadowing-import-from :fdog-control
                          :fdog-interface)

  (:export :init-forwarders))
(in-package :fdog-forwarder)

(defvar *forwarders* ()
  "List of the loaded forwarders")

(clsql:def-view-class fdog-forwarder ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-id)
   (host :type string
         :initform nil)
   (path :type string
         :initform "/")
   (forward-to :type string
               :initform "tcp://localhost:9999"))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

(defclass fdog-forwarding-interface (fdog-interface)
  ((upstream :initarg :upstream
             :accessor forwarder-upstream))
  (:documentation "An interface for forwarding requests to upstream 0mq endpoints."))

(defmethod init-forwarders ()
  "Search for, init and start all known forwarders"
  (log-for (dribble) "Initializing forwarders..")
  (when (not (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder))))
    (log-for (trace) "Forwarder table does not exist.. creating.")
    (clsql:create-view-from-class 'fdog-forwarder))
  :undef)
