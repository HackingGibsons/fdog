(in-package :fdog-models)

;;; Virtual metaclass
(defclass db-with-virtual-slots-class (clsql-sys::standard-db-class) nil)

;;; Virtual slot definition
(defclass virtual-slot-definition (clsql-sys::view-class-slot-definition-mixin standard-slot-definition)
  ((function :initarg :function
             :accessor virtual-slot-definition-function)))

(defmethod slot-definition-allocation ((slotd virtual-slot-definition))
  :virtual)

(defmethod (setf slot-definition-allocation) (allocation (slotd virtual-slot-definition))
  (unless (eq allocation :virtual)
    (error "Cannot change the allocation of ~S" 'virtual-slot-definition))
  allocation)

;;; Virtual direct slot definition
(defclass virtual-direct-slot-definition (clsql-sys::view-class-direct-slot-definition virtual-slot-definition)
  nil)

(defmethod direct-slot-definition-class ((class db-with-virtual-slots-class) &rest initargs)
  (format t "direct-slot-args: Initargs: ~A~%" initargs)
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'virtual-direct-slot-definition)
    (call-next-method)))

(defmethod process-a-slot-option
    ((class db-with-virtual-slots-class) option value already-processed-opts slot)
  (if (eq option :function)
      (list* :function value already-processed-opts)
    (call-next-method)))

;;; Virtual effective slot definition
(defclass virtual-effective-slot-definition (clsql-sys::view-class-effective-slot-definition virtual-slot-definition)
  nil)

(defmethod effective-slot-definition-class ((class db-with-virtual-slots-class) &rest initargs)
  (format t "effective-slot-definition-class<db-with-virtual-slots-class> called.~%")
  (format t "  \- Args: ~A~%" initargs)
  (format t "  \- Name: ~A~%" (getf initargs :name))
  (format t "  \- Alloc: ~A~%" (getf initargs :allocation))
  (let ((slot-alloc (getf initargs :allocation)))
    (format t "  \- Virt? ~A~%"  (eq slot-alloc :virtual))
    (if (eq slot-alloc :virtual)
        (progn
          (format t "  \- seems virtual..~%")
          (find-class 'virtual-effective-slot-definition))
      (call-next-method))))

(defmethod compute-effective-slot-definition ((class db-with-virtual-slots-class) name direct-slot-defs)
  (let ((effective-slotd (call-next-method)))
    (format t "Slot defs: ~A~%" direct-slot-defs)
    (dolist (slotd direct-slot-defs)
      (when (typep slotd 'virtual-direct-slot-definition)
        (format t "Doing things to ~A~%" slotd)
        (format t "  \- Effective: ~A~%" effective-slotd)
        (format t "  \- Initargs: ~A~%" (slot-definition-initargs slotd))
        (setf effective-slotd
              (make-instance 'virtual-effective-slot-definition :name (slot-definition-name slotd)
                                                                :initform (slot-definition-initform slotd)
                                                                :initfunction (slot-definition-initfunction slotd)
                                                                :type (slot-definition-type slotd)
                                                                :allocation (slot-definition-allocation slotd)
                                                                :initargs (slot-definition-initargs slotd)
                                                                :readers (slot-definition-readers slotd)
                                                                :writers (slot-definition-writers slotd)
                                                                :documentation (documentation slotd t))

              (virtual-slot-definition-function effective-slotd)
              (virtual-slot-definition-function slotd))
        (return)))
    (format t "Computed: ~A~%" effective-slotd)
    effective-slotd))

;; Access methods for virtual func access
(defmethod slot-value-using-class ((class db-with-virtual-slots-class) object slot)
  (let ((slotd (find slot (class-slots class) :key 'slot-definition-name)))
    (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function object)
                 :get object)
      (call-next-method))))

(defmethod (setf slot-value-using-class) (value (class db-with-virtual-slots-class) object slot)
  (let ((slotd (find slot (class-slots class) :key 'slot-definition-name)))
        (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function object)
                 :set object value)
      (call-next-method))))

(defmethod slot-boundp-using-class ((class db-with-virtual-slots-class) object slot)
  (let ((slotd (find slot (class-slots class) :key 'slot-definition-name)))
        (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function object)
                 :is-set object)
      (call-next-method))))

(defmethod slot-makunbound-using-class ((class db-with-virtual-slots-class) object slot)
  (let ((slotd (find slot (class-slots class) :key 'slot-definition-name)))
        (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function object)
                 :unset object)
      (call-next-method))))

(defmethod slot-exists-p-using-class ((class db-with-virtual-slots-class) object slot)
  (or (call-next-method)
      (and (find slot (class-slots class) :key 'slot-definition-name)
           t)))

;;; Mongrel2 Configuration models
(clsql:def-view-class mongrel2-server ()
  ((id :type integer :db-kind :key
       :db-constraints '(:unique :auto-increment))
   (uuid :type string)
   (access-log :type string)
   (error-log :type string)
   (chroot :type string
           :initform "/var/www")
   (pid-file :type string)
   (default-host :type string)
   (bind-addr :type string
              :initform "0.0.0.0"))
  (:base-table server
   :documentation
   "Mongrel2 Server configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-260003.4.1"))

(clsql:def-view-class mongrel2-host ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
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

(defun do-all-the-things (action object &optional value)
  (format t "DO ALL THE ~A THINGS ON ~A (Val: ~A)~%" action object value)
  'hallo)

(clsql:def-view-class mongrel2-route ()
  ((path :type string)
   (reversed :type boolean
             :init-form 0)
   (host-id :db-kind :key :type integer)

   (target :db-kind :virtual :allocation :virtual
           :function 'do-all-the-things)

   (target-id :db-kind :key :type integer)    ;; TODO: This relation is not easily expressed in the ORM
   (target-type :db-kind :key :type string))  ;;       needs to be done with a :virtual slot and slot-value-using-class (?)
  (:metaclass db-with-virtual-slots-class)
  (:base-table route
   :documentation
   "Mongrel2 Route configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-280003.4.3"))

(clsql:def-view-class mongrel2-handler ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
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
       :db-constraints '(:unique :auto-increment))
   (base :type string)
   (index-file :type string)
   (default-ctype :type string))
  (:base-table directory
   :documentation
   "Mongrel2 Directory endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-290003.4.4"))

(clsql:def-view-class mongrel2-proxy ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (addr :type string)
   (port :type integer))
  (:base-table proxy
   :documentation
   "Mongrel2 Proxy endpoint configuration: http://mongrel2.org/static/mongrel2-manual.html#x1-300003.4.5"))

(clsql:def-view-class mongrel2-setting ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (key :type string)
   (value :type string))
  (:base-table setting
   :documentation
   "Mongrel2 internal settings: http://mongrel2.org/static/mongrel2-manual.html#x1-380003.10"))

(clsql:def-view-class mongrel2-log ()
  ((id :db-kind :key :type integer
       :db-constraints '(:unique :auto-increment))
   (who :type string)
   (what :type string)
   (location :type string)
   (happened-at :type clsql:wall-time)
   (how :type string)
   (why :type string))
  (:base-table log
   :documentation "Mongrel2 config modification log"))

(clsql:def-view-class mongrel2-mimetype ()
  ((id :db-kind :key :type integer
       :db-constraints '(:primary-key :auto-increment))
   (mimetype :type string)
   (extension :type string))
  (:base-twable mimetype
   :documentation "Mongrel2 table of known mimetypes"))

;; This table exists largely for the future..
;; (maybe)
(clsql:def-view-class mongrel2-statistic ()
  ((id :type integer :db-kind :key
       :db-constraints '(:unique :auto-increment))

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
