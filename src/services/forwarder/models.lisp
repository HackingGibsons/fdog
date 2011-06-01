(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-id)
   (name :type string
         :initarg :name
         :initform "forwarder"
         :accessor fdog-forwarder-name)
   (host :type string
         :initarg :host
         :initform nil)
   (path :type string
         :initarg :path
         :initform "/")
   (listen-on :type string
              :initarg :listen-on
              :initform "tcp://127.0.0.1:9910")
   (forward-to :type string
               :initarg :forward-to
               :initform "tcp://127.0.0.1:9999"))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

(defmethod print-object ((self fdog-forwarder) stream)
  (with-slots (id host path listen-on forward-to) self
    (format stream "#<DBForwarder(~A): ~A~A ~A => ~A>"
            id host path listen-on forward-to)))
