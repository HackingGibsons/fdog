(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder-hostpath ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-host-id)
   (forwarder-id :type integer
                 :reader fdog-forwarder-host-forwarder-id)
   (host :type string
         :initarg :host
         :initform nil)
   (path :type string
         :initarg :path
         :initform "/"
         :accessor fdog-forwarder-path))
  (:base-table fdog-forwarder-hostpath
   :documentation "A host and path pair for a given forwarder."))

(clsql:def-view-class fdog-forwarder ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-id)
   (name :type string
         :initarg :name
         :initform "forwarder"
         :accessor fdog-forwarder-name)
   (listen-on :type integer
              :initarg :listen-on
              :initform 9910)
   (forward-to :type integer
               :initarg :forward-to
               :initform 9999)

   (hostpaths :db-kind :join
              :accessor fdog-forwarder-hostpaths
              :db-info (:join-class fdog-forwarder-hostpath
                        :home-key id
                        :foreign-key forwarder-id
                        :set t)))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

;; Model methods
(defmethod set-forwarder-hostpaths ((forwarder fdog-forwarder) host-paths)
  "Configure the database representation of `forwarder' to include only
the host->path combinations `host-paths' in the form ((''host'' . ''/path/''))"
  (log-for (trace) "Updating forwarder ~A with host-paths ~A" forwarder host-paths)

  (log-for (trace) "Removing existing hostpaths")
  (dolist (host-path (fdog-forwarder-hostpaths forwarder))
    (log-for (trace) "Removing: ~A" host-path)
    (clsql:delete-instance-records host-path))

  (log-for (trace) "Adding new hostpaths")
  (dolist (host-path host-paths)
    (destructuring-bind (host . path) host-path
      (log-for (trace) "Installing: ~A => ~A" host path))))

(defmethod make-forwarder (name &rest host-paths)
  "Make a new forwarder named `name' with hostpaths as configured
in `host-paths' in the form ((host-string . path-string)...)"
  (let ((forwarder (get-or-create-forwarder name)))
    (set-forwarder-hostpaths forwarder host-paths)
    forwarder))

(defmethod get-or-create-forwarder (name)
  (or (find-forwarder :name name :one t)
      (let ((new-forwarder (make-instance 'fdog-forwarder :name name
                                          :forward-to (next-forwarder-port)
                                          :listen-on (next-forwarder-port))))
        (clsql:update-records-from-instance new-forwarder)
        new-forwarder)))

(defmethod find-forwarder (&rest keys &key name one)
  #.(clsql:locally-enable-sql-reader-syntax)
  (cond (one
         (car (apply #'find-forwarder (progn (setf (getf keys :one) nil)
                                             keys))))
        (name
         (clsql:select 'fdog-forwarder :flatp t :refresh t
                       :where [= [slot-value 'fdog-forwarder 'name] name]))
        (t
         (clsql:select 'fdog-forwarder :flatp t :refresh t)))
  #.(clsql:restore-sql-reader-syntax-state))


