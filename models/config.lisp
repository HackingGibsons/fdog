(in-package :fdog-models)
;;; Aux
(let ((endpoints '(("proxy" . mongrel2-proxy)
                   ("handler" . mongrel2-handler)
                   ("dir" . mongrel2-directory))))

  (defun endpoint-by-name (name)
    (cdr (assoc name endpoints :test 'equal)))

  (defun name-by-endpoint (endpoint)
    (car (rassoc (if (typep endpoint 'clsql:standard-db-object)
                     (type-of endpoint) endpoint)
                 endpoints :test 'equal))))

;;; Mongrel2 Configuration models
(clsql:def-view-class mongrel2-server ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :reader mongrel2-server-id
       :db-kind :key)
   (name :type string
         :initarg :name
         :initform "default"
         :accessor mongrel2-server-name)
   (uuid :type string
         :initform (make-uuid4)
         :accessor mongrel2-server-uuid)
   (access-log :type string
               :initform "./logs/{name}-access.log")
   (error-log :type string
               :initform "./logs/{name}-error.log")
   (chroot :type string
           :accessor mongrel2-server-chroot
           :initform "/var/www")
   (pid-file :type string
             :accessor mongrel2-server-pidfile
             :initform "./run/mongrel2-{name}.pid")
   (default-host :type string
                 :initform "localhost"
                 :reader mongrel2-server-default-host)
   (bind-addr :type string
              :initform "0.0.0.0"
              :accessor mongrel2-server-addr)
   (port :type integer
         :initform 6767
         :accessor mongrel2-server-port)

   (root :type string :db-kind :virtual
         :reader mongrel2-server-root
         ;; TODO: Consider the relativity of the chroot slot when computing this slot.
         ;;       or outright move this to a method
         :initform (merge-pathnames fdog:*default-server-path* fdog:*default-root-path*)))
  (:base-table server
   :documentation
   "Mongrel2 Server configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-260003.4.1"))

(clsql:def-view-class mongrel2-host ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader mongrel2-host-id)
   (server-id :type integer)
   (maintenance :type integer :db-type "BOOLEAN"
                :initform 0)
   (name :type string
         :initarg :name
         :accessor mongrel2-host-name)
   (matching :type string
             :name :initarg)

   (server :db-kind :join
           :db-info (:join-class mongrel2-server
                     :home-key server-id
                     :foreign-key id
                     :set nil))

   (routes :db-kind :join
           :reader mongrel2-host-routes
           :db-info (:join-class mongrel2-route
                     :home-key id
                     :foreign-key host-id
                     :set t)))


  (:base-table host
   :documentation
   "Mongrel2 Host configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-270003.4.2"))

(defun complex-join (&key find-model model-field foreign-key (foreign-field 'id))
  (lambda (action object &optional value)
    #.(clsql:locally-enable-sql-reader-syntax)

    (let* ((model-class (funcall find-model (slot-value object model-field)))
           (foreign-key-value (slot-value object foreign-key)))
      (ecase action
        ((:get :is-set)
         (car (clsql:select model-class :flatp t :limit 1
                            :where [= foreign-field foreign-key-value])))

        (:set (let ((name (name-by-endpoint value)) ;; TODO: Factor into &key args
                    (key (slot-value value foreign-field)))
                (setf (slot-value object model-field) name)
                (setf (slot-value object foreign-key) key)
                value))

        (:unset (setf (slot-value object model-field) nil)
                (setf (slot-value object foreign-key) nil))))

    #.(clsql:restore-sql-reader-syntax-state)))

(clsql:def-view-class mongrel2-route ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (path :type string
         :accessor mongrel2-route-path
         :initarg :path
         :initform "/")
   (reversed :type integer :db-type "BOOLEAN"
             :initarg :reversed
             :initform 0)
   (host-id :type integer)

   (target :db-kind :virtual :allocation :virtual
           :function (complex-join :find-model 'endpoint-by-name
                                   :model-field 'target-type
                                   :foreign-key 'target-id))


   (target-id :type integer :initform nil)
   (target-type :type string :initform nil))
  (:metaclass db-with-virtual-slots-class)
  (:base-table route
   :documentation
   "Mongrel2 Route configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-280003.4.3"))

(clsql:def-view-class mongrel2-handler ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (send-spec :type string
              :initarg :send-spec)
   (send-ident :type string
               :initarg :send-ident)
   (recv-spec :type string
              :initarg :recv-spec)
   (recv-ident :type string
               :initarg :recv-ident
               :initform "")
   (raw-payload :type integer
                :initform 0)
   (protocol :type string
             :initform "json"))
  (:base-table handler
   :documentation
   "Mongrel2 Handler endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-310003.4.6"))

(clsql:def-view-class mongrel2-directory ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (base :type string
         :initarg :base)
   (index-file :type string
               :initarg :index
               :initform "index.html")
   (default-ctype :type string
                  :initarg :default-ctype
                  :initform "text/html"))
  (:base-table directory
   :documentation
   "Mongrel2 Directory endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-290003.4.4"))

(clsql:def-view-class mongrel2-proxy ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (addr :type string
         :initarg :addr)
   (port :type integer
         :initarg :port))
  (:base-table proxy
   :documentation
   "Mongrel2 Proxy endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-300003.4.5"))

(clsql:def-view-class mongrel2-setting ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (key :type string)
   (value :type string))
  (:base-table setting
   :documentation
   "Mongrel2 internal settings: http://mongrel2.org/static/mongrel2-manual.html#x1-380003.10"))

(clsql:def-view-class mongrel2-log ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (who :type string)
   (what :type string)
   (location :type string)
   (happened-at :type clsql:wall-time)
   (how :type string)
   (why :type string))
  (:base-table log
   :documentation "Mongrel2 config modification log"))

(clsql:def-view-class mongrel2-mimetype ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (mimetype :type string)
   (extension :type string))
  (:base-table mimetype
   :documentation "Mongrel2 table of known mimetypes"))

;; This table exists largely for the future..
;; (maybe)
(clsql:def-view-class mongrel2-statistic ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)

   ;; These /should/ be a c-pk
   (other-type :type string)
   (other-id :type integer)
   (name :type string)
   ;; End of c-pk

   (sum :type float)
   (sumsq :type float)
   (n :type integer)
   (min :type float)
   (max :type float)
   (mean :type float)
   (sd :type float))
  (:base-table statistic
   :documentation "Mongrel2 perfomance statistics table. Unused as of writing."))
