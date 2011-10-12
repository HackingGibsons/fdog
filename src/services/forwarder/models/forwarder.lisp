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

(defmethod delete-forwarder ((forwarder fdog-forwarder))
  (with-slots (name) forwarder
    (and (find-forwarder :name name)
         (clsql:delete-instance-records forwarder))))

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

;; Predicates
(defmethod forwarder-valid-p ((forwarder fdog-forwarder))
  "Returns identity of forwarder or nil if the forwarder is not valid
to use."
  (and (< 0 (length (fdog-forwarder-hostpaths forwarder)))))

