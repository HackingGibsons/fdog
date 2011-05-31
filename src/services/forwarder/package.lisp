(defpackage #:fdog-forwarder
  (:use #:cl)
  (:shadowing-import-from :log5 :log-for)

  (:shadowing-import-from :fdog-models
                          :model-pk
                          :mongrel2-server
                          :mongrel2-server-default-host
                          :mongrel2-server-default-host-name
                          :mongrel2-host
                          :mongrel2-host-server)
  (:shadowing-import-from :fdog-control
                          :api/endpoint)
  (:shadowing-import-from :fdog-m2sh
                          :servers :make-server)
  (:shadowing-import-from :fdog-control
                          :interface-start :interface-stop
                          :interface-configure-bridges
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

(defmethod print-object ((self fdog-forwarder) stream)
  (with-slots (id host path listen-on forward-to) self
    (format stream "#<DBForwarder(~A): ~A~A ~A => ~A>"
            id host path listen-on forward-to)))

(defclass fdog-forwarding-interface (fdog-interface)
  ((upstream :initarg :upstream
             :accessor forwarder-upstream)
   (context :initform nil
            :initarg :context
            :accessor forwarder-context)
   (response-write-sock :initform nil
                        :initarg :response-write-sock
                        :accessor forwarder-response-write-sock)
   (response-sock :initform nil
                :initarg :listen-sock
                :accessor forwarder-listen-sock)
   (request-sock :initform nil
                 :initarg :request-sock
                 :accessor forwarder-request-sock)
   (response-writer :initform nil
                    :accessor forwarding-interface-response-writer))
  (:documentation "An interface for forwarding requests to upstream 0mq endpoints."))

(defmethod interface-start :before ((self fdog-forwarding-interface))
  (log-for (trace) "Starting forwarder ~A" self))

(defmethod interface-stop :before ((self fdog-forwarding-interface))
  (log-for (trace) "Stopping forwarder ~A" self))

(defmethod initialize-instance :after ((self fdog-forwarding-interface) &rest initargs)
  "Initialize the forwarder"
  (declare (ignorable initargs))
  :undef)

(defmethod make-forwarder-interface ((forwarder fdog-forwarder))
  (log-for (trace) "Making interface from forwarder ~A" forwarder)
  (make-instance 'fdog-forwarding-interface
                 :server (ensure-server-exists "forwarder" 13374)
                 :upstream forwarder))

(defun ensure-server-exists (name port &key (bind "0.0.0.0"))
  (log-for (trace) "Making sure we have a server named ~A binding to ~A" name port)
  (let ((server (or (servers :name name :refresh t :one t)
                    (make-server name :port port :bind bind))))
    (log-for (dribble) "Server with name so far: ~A" server)
    (ensure-server-default-host-exists server)

    server))

(defmethod ensure-server-default-host-exists ((server mongrel2-server))
  (log-for (trace) "Making sure we have a default host for ~A" server)
  (let* ((host (mongrel2-server-default-host server))
         (correct (and host
                       (= (model-pk server)
                          (model-pk (mongrel2-host-server host))))))
    (unless correct
      (setf host (make-instance 'mongrel2-host
                                :name (mongrel2-server-default-host-name server)
                                :server-id (model-pk server)))
      (clsql:update-records-from-instance host))
    host))

(defmethod init-forwarders ()
  "Search for, init and start all known forwarders"
  (log-for (dribble) "Initializing forwarders..")
  (when (not (clsql:table-exists-p (clsql:view-table (find-class 'fdog-forwarder))))
    (log-for (trace) "Forwarder table does not exist.. creating.")
    (clsql:create-view-from-class 'fdog-forwarder))

  ;; Turn off any that we already have running
  (log-for (trace) "Removing current forwarders..")
  (setf *forwarders*
        (dolist (forwarder *forwarders* nil)
          (when (typep forwarder 'fdog-forwarder)
            (interface-stop forwarder))
          (log-for (trace) "Removed: ~A" forwarder)))

  (log-for (trace) "Adding forwarders..")
  (setf *forwarders*
        (mapcar #'make-forwarder-interface
                (clsql:select 'fdog-forwarder :flatp t :refresh t))))
