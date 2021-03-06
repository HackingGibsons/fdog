(in-package :fdog-models)
;;; Aux
(let ((endpoints (list (cons "proxy" 'mongrel2-proxy)
                       (cons "handler"  'mongrel2-handler)
                       (cons "dir"  'mongrel2-directory))))

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
   (use-ssl :type integer
            :initform 0
            :initarg :use-ssl
            :accessor mongrel2-server-ssl)
   (access-log :type string
               :initform "/logs/{name}-access.log")
   (error-log :type string
               :initform "/logs/{name}-error.log")
   (chroot :type string
           :initarg :chroot
           :accessor mongrel2-server-chroot
           :initform "./")
   (pid-file :type string
             :accessor mongrel2-server-pidfile
             :initform "/run/mongrel2-{name}.pid")
   (default-host :type string
                 :initform "localhost"
                 :initarg :default-host
                 :reader mongrel2-server-default-host-name)
   (bind-addr :type string
              :initarg :bind
              :initform "0.0.0.0"
              :accessor mongrel2-server-addr)
   (port :type integer
         :initarg :port
         :initform 6767
         :accessor mongrel2-server-port)

   (hosts :db-kind :join
          :accessor mongrel2-server-hosts
          :db-info (:join-class mongrel2-host
                    :home-key id
                    :foreign-key server-id
                    :set t))

   (root :type string :db-kind :virtual
         :reader mongrel2-server-root
         ;; TODO: Consider the relativity of the chroot slot when computing this slot.
         ;;       or outright move this to a method
         :initform (merge-pathnames fdog:*default-server-path* fdog:*root-path*)))
  (:base-table server
   :documentation
   "Mongrel2 Server configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-260003.4.1"))

(defmethod print-object ((server mongrel2-server) s)
  "Pretty-printer of server"
  (format s "#<Mongrel2-Server(~A) ~A/~A SSL: ~[No~;Yes~] Running: ~:[No~;Yes~]>"
          (if (slot-boundp server 'id) (model-pk server) "None")
          (if (slot-boundp server 'name) (mongrel2-server-name server) "[None]")
          (if (slot-boundp server 'uuid) (mongrel2-server-uuid server) "[None]")
          (mongrel2-server-ssl server)
          (mongrel2-server-running-p server)))

(clsql:def-view-class mongrel2-host ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader mongrel2-host-id)
   (server-id :type integer
              :initarg :server-id)
   (maintenance :type integer :db-type "BOOLEAN"
                :initform 0)
   (name :type string
         :initarg :name
         :accessor mongrel2-host-name)
   (matching :type string
             :initarg :matching
             :accessor mongrel2-host-matching)

   (server :db-kind :join
           :accessor mongrel2-host-server
           :db-info (:join-class mongrel2-server
                     :home-key server-id
                     :foreign-key id
                     :set nil))

   (routes :db-kind :join
           :reader mongrel2-host-routes-set
           :db-info (:join-class mongrel2-route
                     :home-key id
                     :foreign-key host-id
                     :set t)))


  (:base-table host
   :documentation
   "Mongrel2 Host configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-270003.4.2"))

(defmethod find-mongrel2-host ((server mongrel2-server) name)
  #.(clsql:locally-enable-sql-reader-syntax)
  (car (clsql:select 'mongrel2-host :flatp t :refresh t
                     :where [and [= [slot-value 'mongrel2-host 'server-id]
                                    (model-pk server)]
                                 [= [slot-value 'mongrel2-host 'name]
                                    name]]))
  #.(clsql:restore-sql-reader-syntax-state))

(defmethod make-mongrel2-host ((server mongrel2-server) name)
  (let ((host (or (find-mongrel2-host server name)
                  (make-instance 'mongrel2-host :server-id (model-pk server)
                                 :name name))))
    (clsql:update-records-from-instance host)
    host))

(defmethod print-object ((host mongrel2-host) stream)
  (with-slots (id name matching routes) host
    (format stream "#<Host(~A:~A)::~A ~A route>" id name matching (length routes))))

(defun complex-join (&key find-model model-field foreign-key (foreign-field 'id))
  (lambda (action object &optional value)
    #.(clsql:locally-enable-sql-reader-syntax)

    (let* ((model-class (funcall find-model (slot-value object model-field)))
           (foreign-key-value (slot-value object foreign-key)))
      (ecase action
        ((:get :is-set)
         (when model-class
           (car (clsql:select model-class :flatp t :limit 1 :refresh t
                              :where [= foreign-field foreign-key-value]))))

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
   (host-id :type integer
            :initarg :host-id)
   (host :db-kind :join
         :accessor mongrel2-route-host
         :db-info (:join-class mongrel2-host
                   :home-key host-id
                   :foreign-key id
                   :set nil))

   (target :db-kind :virtual :allocation :virtual
           :accessor mongrel2-route-target
           :function (complex-join :find-model 'endpoint-by-name
                                   :model-field 'target-type
                                   :foreign-key 'target-id))


   (target-id :type integer :initform nil)
   (target-type :type string :initform nil))
  (:metaclass db-with-virtual-slots-class)
  (:base-table route
   :documentation
   "Mongrel2 Route configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-280003.4.3"))

(defmethod print-object ((route mongrel2-route) stream)
  (with-slots (id reversed path target-type target-id) route
    (format stream "#<Route(~A)::~:[~;(reversed)~]~A => ~A(~A)>"
            (and (slot-boundp route 'id) id)
            (/= reversed 0) path target-type target-id)))

(defmethod find-mongrel2-route ((host mongrel2-host) (path string))
  "Find a mongrel2-route attached to the given `host' that have a path given
by `path'"
  #.(clsql:locally-enable-sql-reader-syntax)
  (car (clsql:select 'mongrel2-route :flatp t :refresh t
                     :where [and [= [slot-value 'mongrel2-route 'path]
                                    path]
                                 [= [slot-value 'mongrel2-route 'host-id]
                                    (model-pk host)]]))
  #.(clsql:restore-sql-reader-syntax-state))



;; TODO: The closures mapping targets at the top should
;;       probably be methods spcialized on target :|
(defclass mongrel2-target nil nil
  (:documentation "A base class for directory targets"))

(clsql:def-view-class mongrel2-handler (mongrel2-target)
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key)
   (send-spec :type string
              :accessor mongrel2-handler-send-spec
              :initarg :send-spec)
   (send-ident :type string
               :accessor mongrel2-handler-send-ident
               :initarg :send-ident)
   (recv-spec :type string
              :accessor mongrel2-handler-recv-spec
              :initarg :recv-spec)
   (recv-ident :type string
               :accessor mongrel2-handler-recv-ident
               :initarg :recv-ident
               :initform "")
   (raw-payload :type integer
                :initform 0)
   (protocol :type string
             :accessor mongrel2-handler-protocol
             :initform "json"))
  (:base-table handler
   :documentation
   "Mongrel2 Handler endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-310003.4.6"))

(defun find-mongrel2-handler (&key send-ident)
  "Find either all handlers, or a handler with the given `send-ident'"
  #.(clsql:locally-enable-sql-reader-syntax)
  (cond ((not send-ident)
         (clsql:select 'mongrel2-handler :flatp t :refresh t))
        (t
         (car (clsql:select 'mongrel2-handler :flatp t :refresh t
                            :where [= [slot-value 'mongrel2-handler 'send-ident]
                                      send-ident]))))
  #.(clsql:restore-sql-reader-syntax-state))


(defun make-mongrel2-handler (send-ident send-spec recv-spec &key (recv-ident "") (update t))
  "Makes or updates a handler with the send-ident of `send-ident'
setting the `send-spec' and `recv-spec'"
  (let ((handler (or (find-mongrel2-handler :send-ident send-ident)
                     (make-instance 'mongrel2-handler :send-ident send-ident))))

    (when (or (not (slot-boundp handler 'fdog-models::id))
              update)
      (setf (mongrel2-handler-recv-ident handler) recv-ident
            (mongrel2-handler-send-spec handler) send-spec
            (mongrel2-handler-recv-spec handler) recv-spec))

    (clsql:update-records-from-instance handler)
    handler))



(defmethod print-object ((handler mongrel2-handler) stream)
  (with-slots (id protocol recv-ident recv-spec send-ident send-spec) handler
  (format stream "#<Handler(~A):~A::[recv(~A):~A] => [send(~A):~A]>"
          id protocol recv-ident recv-spec send-ident send-spec)))

(clsql:def-view-class mongrel2-directory (mongrel2-target)
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
                  :initform "text/html")
   (cache-ttl :type integer :initform 0
              :initarg :cache-ttl))
  (:base-table directory
   :documentation
   "Mongrel2 Directory endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-290003.4.4"))

(clsql:def-view-class mongrel2-proxy (mongrel2-target)
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
   (key :type string
        :initarg :key
        :accessor mongrel2-setting-key)
   (value :type string
          :initarg :value
          :accessor mongrel2-setting-value))
  (:base-table setting
   :documentation
   "Mongrel2 internal settings: http://mongrel2.org/static/mongrel2-manual.html#x1-380003.10"))

(defmethod print-object ((object mongrel2-setting) s)
  "Pretty printer for `mongrel2-setting'"
  (format s "#<Mongrel2-Setting[~A]: ~A => ~A>"
          (if (slot-boundp object 'id) (model-pk object) "None")
          (if (slot-boundp object 'key) (mongrel2-setting-key object) "[None]")
          (if (slot-boundp object 'value) (mongrel2-setting-value object) "[None]")))

(defmethod make-mongrel2-setting ((key symbol) value)
  "Helper to convert symbol `key's to downcased string keys"
  (make-mongrel2-setting (string-downcase (symbol-name key)) value))

(defmethod make-mongrel2-setting (key value)
  "Get or create a setting with key `key' and set the value to `value'"
  (let ((setting (or (find-mongrel2-setting key)
                     (make-instance 'mongrel2-setting
                                    :key key))))
    (setf (mongrel2-setting-value setting) value)
    (clsql:update-records-from-instance setting)
    setting))

(defmethod find-mongrel2-setting ((key symbol))
  (find-mongrel2-setting (string-downcase (symbol-name key))))

(defmethod find-mongrel2-setting (key)
  #.(clsql:locally-enable-sql-reader-syntax)
  (car (clsql:select 'mongrel2-setting :flatp t :refresh t
                     :where [= [slot-value 'mongrel2-setting 'key]
                               key]))
  #.(clsql:restore-sql-reader-syntax-state))


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
