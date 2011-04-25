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
  ((id :type integer :db-kind :key
       :reader mongrel2-server-id
       :db-constraints (:unique))
   (name :type string
         :accessor mongrel2-server-name)
   (uuid :type string
         :accessor mongrel2-server-uuid)
   (access-log :type string)
   (error-log :type string)
   (chroot :type string
           :accessor mongrel2-server-chroot
           :initform "/var/www")
   (pid-file :type string
             :accessor mongrel2-server-pidfile)
   (default-host :type string
                 :reader mongrel2-server-default-host)
   (bind-addr :type string
              :accessor mongrel2-server-addr
              :initform "0.0.0.0")
   (port :type integer
         :accessor mongrel2-server-port
         :initform 6767)

   (root :type string :db-kind :virtual
         :reader mongrel2-server-root
         ;; TODO: Consider the relativity of the chroot slot when computing this slot.
         ;;       or outright move this to a method
         :initform (merge-pathnames fdog:*default-server-path* fdog:*default-root-path*)))
  (:base-table server
   :documentation
   "Mongrel2 Server configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-260003.4.1"))

(clsql:def-view-class mongrel2-host ()
  ((id :db-kind :key :type integer
       :db-constraints (:unique))
   (server-id :type integer :db-kind :key)
   (maintenance :type boolean)
   (name :type string)
   (matching :type string)

   (server :db-kind :join
           :db-info (:join-class mongrel2-server
                     :home-key server-id
                     :foreign-key id
                     :set nil)))
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
  ((path :type string)
   (reversed :type boolean
             :init-form 0)
   (host-id :db-kind :key :type integer)

   (target :db-kind :virtual :allocation :virtual
           :function (complex-join :find-model 'endpoint-by-name
                                   :model-field 'target-type
                                   :foreign-key 'target-id))


   (target-id :db-kind :key :type integer)    ;; TODO: This relation is not easily expressed in the ORM
   (target-type :db-kind :key :type string))  ;;       needs to be done with a :virtual slot and slot-value-using-class (?)
  (:metaclass db-with-virtual-slots-class)
  (:base-table route
   :documentation
   "Mongrel2 Route configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-280003.4.3"))

(clsql:def-view-class mongrel2-handler ()
  ((id :db-kind :key :type integer)
    (send-spec :type string)
    (send-ident :type string)
    (recv-spec :type string)
    (recv-ident :type string)
    (raw-payload :type integer
                 :initform 0)
    (protocol :type string
              :initform "json"))
  (:base-table handler
   :documentation
   "Mongrel2 Handler endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-310003.4.6"))

(clsql:def-view-class mongrel2-directory ()
  ((id :db-kind :key :type integer
       :db-constraints (:unique))
   (base :type string)
   (index-file :type string)
   (default-ctype :type string))
  (:base-table directory
   :documentation
   "Mongrel2 Directory endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-290003.4.4"))

(clsql:def-view-class mongrel2-proxy ()
  ((id :db-kind :key :type integer
       :db-constraints (:unique))
   (addr :type string)
   (port :type integer))
  (:base-table proxy
   :documentation
   "Mongrel2 Proxy endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-300003.4.5"))

(clsql:def-view-class mongrel2-setting ()
  ((id :db-kind :key :type integer
       :db-constraints (:unique))
   (key :type string)
   (value :type string))
  (:base-table setting
   :documentation
   "Mongrel2 internal settings: http://mongrel2.org/static/mongrel2-manual.html#x1-380003.10"))

(clsql:def-view-class mongrel2-log ()
  ((id :db-kind :key :type integer
       :db-constraints (:unique))
   (who :type string)
   (what :type string)
   (location :type string)
   (happened-at :type clsql:wall-time)
   (how :type string)
   (why :type string))
  (:base-table log
   :documentation "Mongrel2 config modification log"))

(clsql:def-view-class mongrel2-mimetype ()
  ((id :db-kind :key :type integer)
   (mimetype :type string)
   (extension :type string))
  (:base-table mimetype
   :documentation "Mongrel2 table of known mimetypes"))

;; This table exists largely for the future..
;; (maybe)
(clsql:def-view-class mongrel2-statistic ()
  ((id :type integer :db-kind :key
       :db-constraints (:unique))

   ;; These /should/ be a c-pk
   (other-type :db-kind :key :type string)
   (other-id :db-kind :key :type integer)
   (name :db-kind :key :type string)
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
