(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder-queue ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-queue-id)
   (forwarder-id :type integer
                 :initarg :forwarder-id
                 :reader fdog-forwarder-queue-forwarder-id)
   (enabled :type boolean
            :accessor forwarder-queue-enabled
            :initarg :enabled
            :initform t)
   (depth :type integer
          :accessor forwarder-queue-depth
          :initarg :limit
          :initform nil))
  (:base-table fdog-forwarder-queue
   :documentation "Any applicable queue options for a given forwarder."))

(defmethod print-object ((object fdog-forwarder-queue) s)
  (format s "#<ForwarderQueue[~A]: Forwarder[~A] ~:[Disabled~;Enabled~] Depth: ~A>"
          (if (slot-boundp object 'id) (fdog-forwarder-queue-id object) "None")
          (if (slot-boundp object 'forwarder-id) (fdog-forwarder-queue-forwarder-id object) "None")
          (forwarder-queue-enabled object) (forwarder-queue-depth object)))



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

(clsql:def-view-class fdog-forwarder-alias ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-alias-id)
   (forwarder-id :type integer
                 :initarg :forwarder-id
                 :reader fdog-forwarder-host-forwarder-id)
   (name :type string
         :initarg :name)

   (match :type string
          :initarg :match
          :accessor fdog-forwarder-alias-match
          :initform "^$")
   (method :type string
           :initarg :method
           :accessor fdog-forwarder-alias-method
           :initform nil)

   (listen-on :type integer
              :accessor fdog-forwarder-alias-listen-on)
   (forward-to :type integer
               :accessor fdog-forwarder-alias-forward-to)

   (forwarder :db-kind :join
              :accessor fdog-forwarder-alias-forwarder
              :db-info (:join-class fdog-forwarder
                        :home-key forwarder-id
                        :foreign-key id
                        :set nil)))

  (:base-table fdog-forwarder-alias
   :documentation "Database model for handling regex-segregated clusters of an app."))

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
              :accessor fdog-forwarder-listen-on
              :initarg :listen-on
              :initform 9910)
   (forward-to :type integer
               :accessor fdog-forwarder-forward-to
               :initarg :forward-to
               :initform 9999)

   (queue-options :db-kind :join
                  :accessor fdog-forwarder-queue-options
                  :db-info (:join-class fdog-forwarder-queue
                            :home-key id
                            :foreign-key forwarder-id
                            :set nil))

   (aliases   :db-kind :join
              :accessor fdog-forwarder-aliases
              :db-info (:join-class fdog-forwarder-alias
                        :home-key id
                        :foreign-key forwarder-id
                        :set t))

   (hostpaths :db-kind :join
              :accessor fdog-forwarder-hostpaths
              :db-info (:join-class fdog-forwarder-hostpath
                        :home-key id
                        :foreign-key forwarder-id
                        :set t)))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

(defmethod print-object ((forwarder fdog-forwarder) s)
  "Pretty printer for `fdog-forwarder' class"
  (format s "#<Forwarder '~A': ~A hostpaths>"
          (fdog-forwarder-name forwarder)
          (length (fdog-forwarder-hostpaths forwarder))))

;; Makers
(defmethod find-forwarder-alias ((forwarder fdog-forwarder) name)
  #.(clsql:locally-enable-sql-reader-syntax)
  (car (clsql:select 'fdog-forwarder-alias :flatp t :refresh t
                     :where [and [= [slot-value 'fdog-forwarder-alias 'forwarder-id]
                                    (fdog-forwarder-id forwarder)]
                                 [= [slot-value 'fdog-forwarder-alias 'name]
                                    name]]))
  #.(clsql:restore-sql-reader-syntax-state))

(defmethod get-or-create-forwarder-alias ((forwarder fdog-forwarder) (name string))
  "Get an instance of or create and persist a forwarder alias for forwarder `forwarder'
named `namee'"
  (or (find-forwarder-alias forwarder name)
      (let ((instance (make-instance 'fdog-forwarder-alias
                                     :forwarder-id (fdog-forwarder-id forwarder)
                                     :name name)))
        (clsql:update-records-from-instance instance)
        instance)))

(defmethod make-forwarder-alias ((forwarder fdog-forwarder) (name string) &key match method)
  "Make an alias for the forwarder `forwarder' named `name' If `match' and/or `method' are
added, they will be set, otherwise the alias will match the empty string (^$)"
  (let ((alias (get-or-create-forwarder-alias forwarder name)))
    (when match
      (setf (fdog-forwarder-alias-match alias) match))
    (when method
      (setf (fdog-forwarder-alias-method alias) method))
    alias))


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

(defmethod make-forwarder ((name symbol) &rest host-paths)
  "Helper to translate keywords to name slugs"
  (call-next-method (string-downcase (symbol-name name)) host-paths))

(defmethod make-forwarder ((name string) &rest host-paths)
  "Make a new forwarder named `name' with hostpaths as configured
in `host-paths' in the form ((host-string . path-string)...)"
  (let ((forwarder (get-or-create-forwarder name)))
    (set-forwarder-hostpaths forwarder host-paths)
    (make-forwarder-queue-option forwarder)
    forwarder))

(defmethod get-or-create-forwarder (name)
  "Either find or create an fdog-forwarder object instance with
a database backing"
  (or (find-forwarder :name name :one t)
      (let ((new-forwarder (make-instance 'fdog-forwarder :name name
                                          :forward-to (next-forwarder-port)
                                          :listen-on (next-forwarder-port))))
        (clsql:update-records-from-instance new-forwarder)
        new-forwarder)))

(defmethod find-forwarder (&rest keys &key name one)
  "Search for forwarder(s) filtering using the key `:name' to filter only interesting names
and the key `:one' to (car *) the operation for convinience, otherwise returns list."
  #.(clsql:locally-enable-sql-reader-syntax)
  (cond (one
         (car (apply #'find-forwarder (progn (setf (getf keys :one) nil)
                                             keys))))
        (name
         (clsql:select 'fdog-forwarder :flatp t :refresh t
                       :where [= [slot-value 'fdog-forwarder 'name]
                                  (if (symbolp name)
                                      (string-downcase (symbol-name name))
                                      name)]))
        (t
         (clsql:select 'fdog-forwarder :flatp t :refresh t)))
  #.(clsql:restore-sql-reader-syntax-state))

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

;; Predicates
(defmethod forwarder-valid-p ((forwarder fdog-forwarder))
  "Returns identity of forwarder or nil if the forwarder is not valid
to use."
  (and (< 0 (length (fdog-forwarder-hostpaths forwarder)))))

;; Queries
(defmethod forwarder-uniqe-paths ((forwarder fdog-forwarder))
  "Return a list of all uniqe paths for a given `forwarder'
Used to build a set of specific handlers pre-computed to
remove a specifc path before sending to a unified upstream"
  (remove-duplicates
   (mapcar #'fdog-hostpath-path
           (fdog-forwarder-hostpaths forwarder))
   :test #'string=))


;; Queue related
(defmethod make-forwarder-queue-option ((forwarder fdog-forwarder))
  "Fetch an existing, or make a new `fdog-forwarder-queue' object
for a given `forwarder'"
  (clsql:update-objects-joins `(,forwarder))
  (let ((q-option (or (fdog-forwarder-queue-options forwarder)
                      (make-instance 'fdog-forwarder-queue
                                     :forwarder-id (fdog-forwarder-id forwarder)))))
    (setf (fdog-forwarder-queue-options forwarder) q-option)
    (clsql:update-records-from-instance q-option)
    q-option))

(defmethod fdog-forwarder-queue-options :around ((forwarder fdog-forwarder))
  "Ensure the freshest copy of the `forwarder' coming out when this method is called.
TODO: Paranoid precaution smells."
  (let ((q-opt (call-next-method)))
    (when q-opt
      (clsql:update-instance-from-records q-opt))
    q-opt))

(defmethod forwarder-queue-enable ((forwarder fdog-forwarder) &key depth)
  "Enable queueing on this `forwarder', next init requests will flow into redis."
  (log-for (trace) "Enabling request queue for ~A" forwarder)
  (let ((q-opt (make-forwarder-queue-option forwarder)))
    (setf (forwarder-queue-enabled q-opt) t
          (forwarder-queue-depth q-opt) depth)
    (clsql:update-records-from-instance q-opt))
  forwarder)


(defmethod forwarder-queue-disable ((forwarder fdog-forwarder))
  "Turn off queueing on this `forwarder', next init requests will no
longer flow to redis.
TODO: Consider what happens to outstanding queue requests."
  (log-for (trace) "Disabling request queueing for ~A" forwarder)
  (let ((q-opt (make-forwarder-queue-option forwarder)))
    (setf (forwarder-queue-enabled q-opt) nil)
    (clsql:update-records-from-instance q-opt))
  forwarder)

(defmethod forwarder-queuing-p ((forwarder fdog-forwarder))
  "Is the our `forwarder' queuing?"
  (let ((q-opt (make-forwarder-queue-option forwarder)))
    (and (forwarder-queue-enabled q-opt)
         (or (not (forwarder-queue-depth q-opt))
             (> 0 (forwarder-queue-depth q-opt))))))
