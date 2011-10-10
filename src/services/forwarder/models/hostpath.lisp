(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder-hostpath ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-host-id)
   (forwarder-id :type integer
                 :initarg :forwarder-id
                 :reader fdog-forwarder-host-forwarder-id)
   (host :type string
         :accessor fdog-hostpath-host
         :initarg :host
         :initform nil)
   (path :type string
         :initarg :path
         :initform "/"
         :accessor fdog-hostpath-path)

   (forwarder :db-kind :join
              :accessor hostpath-forwarder
              :db-info (:join-class fdog-forwarder
                        :home-key forwarder-id
                        :foreign-key id
                        :set nil)))

  (:base-table fdog-forwarder-hostpath
   :documentation "A host and path pair for a given forwarder."))

(defmethod print-object ((hostpath fdog-forwarder-hostpath) s)
  "Pretty printer for `fdog-forwarder-hostpath' class"
  (format s "#<Hostpath(~A): ~A~A" (if (slot-boundp hostpath 'id)
                                       (fdog-forwarder-name (hostpath-forwarder hostpath))
                                       "None")
          (fdog-hostpath-host hostpath) (fdog-hostpath-path hostpath)))

(defmethod make-forwarder-hostpath ((forwarder fdog-forwarder) host path &key (search :host))
  "Make a hostpath for the forwarder `forwarder', if an entry exists for this host
it is overriden with the given path.  Nothing created if already exists.
TODO: use the `search' keyword, it does not currently alter flow."
  (declare (ignorable search))
  (clsql:update-objects-joins `(,forwarder))
  (let* ((hostpaths (fdog-forwarder-hostpaths forwarder))
         (hostpath (or (find host hostpaths :key #'fdog-hostpath-host
                                            :test #'string=)
                       (make-instance 'fdog-forwarder-hostpath
                                      :forwarder-id (fdog-forwarder-id forwarder)
                                      :host host :path path))))
    (log-for (trace) "Made path: ~A" hostpath)
    (setf (fdog-hostpath-path hostpath) path)
    (clsql:update-records-from-instance hostpath)
    hostpath))

;; Construction and update utils
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
      (log-for (trace) "Installing: ~A => ~A" host path)
      (make-forwarder-hostpath forwarder host path)))

  (log-for (trace) "Refreshing instance and joins")
  (clsql:update-instance-from-records forwarder)
  (clsql:update-objects-joins `(,forwarder))
  forwarder)

(defmethod fdog-forwarder-hostpaths :before ((forwarder fdog-forwarder))
  "Force an update of the object joins when we use the accessor"
  (clsql:update-objects-joins `(,forwarder)))

;; Queries
(defmethod forwarder-uniqe-paths ((forwarder fdog-forwarder))
  "Return a list of all uniqe paths for a given `forwarder'
Used to build a set of specific handlers pre-computed to
remove a specifc path before sending to a unified upstream"
  (remove-duplicates
   (mapcar #'fdog-hostpath-path
           (fdog-forwarder-hostpaths forwarder))
   :test #'string=))

