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
         :initarg :host
         :initform nil)
   (path :type string
         :initarg :path
         :initform "/")
   (listen-on :type string
              :initarg :listen-on
              :initform "tcp://localhost:9910")
   (forward-to :type string
               :initarg :forward-to
               :initform "tcp://localhost:9999"))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

(defclass fdog-forwarding-interface (fdog-interface)
  ((upstream :initarg :upstream
             :accessor forwarder-upstream)
   (context :initform nil
            :initarg :context
            :accessor forwarder-context)
   (response-sock :initform nil
                :initarg :listen-sock
                :accessor forwarder-listen-sock)
   (request-sock :initform nil
                 :initarg :request-sock
                 :accessor forwarder-request-sock)
   (response-writer :initform nil
                    :accessor forwarding-interface-response-writer))
  (:documentation "An interface for forwarding requests to upstream 0mq endpoints."))



(defmethod initialize-instance :after ((self fdog-forwarding-interface) &rest initargs)
  "Initialize the forwarder"
  (declare (ignorable initargs))
  :undef)

(defmethod init-forwarders ()
  "Search for, init and start all known forwarders"
  (log-for (dribble) "Initializing forwarders..")
  (when (not (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder))))
    (log-for (trace) "Forwarder table does not exist.. creating.")
    (clsql:create-view-from-class 'fdog-forwarder))
  :undef)
