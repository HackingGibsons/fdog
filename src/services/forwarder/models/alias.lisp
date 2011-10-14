(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder-alias ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-alias-id)
   (forwarder-id :type integer
                 :initarg :forwarder-id
                 :reader fdog-forwarder-alias-forwarder-id)
   (name :type string
         :accessor fdog-forwarder-alias-name
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
              :initarg :listen-on
              :accessor fdog-forwarder-alias-listen-on)
   (forward-to :type integer
               :initarg :forward-to
               :accessor fdog-forwarder-alias-forward-to)

   (forwarder :db-kind :join
              :accessor fdog-forwarder-alias-forwarder
              :db-info (:join-class fdog-forwarder
                        :retrieval :immediate
                        :home-key forwarder-id
                        :foreign-key id
                        :set nil)))

  (:base-table fdog-forwarder-alias
   :documentation "Database model for handling regex-segregated clusters of an app."))


(defmethod print-object ((object fdog-forwarder-alias) s)
  (format s "#<ForwarderAlias[~A]: ~A => ~A::~A>"
          (if (slot-boundp object 'forwarder-id) (fdog-forwarder-alias-forwarder-id object) "None")
          (if (slot-boundp object 'name) (fdog-forwarder-alias-name object) "None")
          (or (fdog-forwarder-alias-method object) "AnyMethod")
          (if (slot-boundp object 'match) (fdog-forwarder-alias-match object) "None")))

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
                                     :name name
                                     :listen-on (next-forwarder-port)
                                     :forward-to (next-forwarder-port))))
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
    (clsql:update-records-from-instance alias)
    alias))

(defmethod update-forwarder-alias ((alias fdog-forwarder-alias) &key name method match)
  (when name
    (setf (fdog-forwarder-alias-name alias) name))
  (when method
    (setf (fdog-forwarder-alias-method alias) method))
  (when match
    (setf (fdog-forwarder-alias-match alias) match))
  (clsql:update-records-from-instance alias)
  alias)
