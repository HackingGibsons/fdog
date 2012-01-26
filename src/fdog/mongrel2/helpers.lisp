(in-package :fdog-models)

(defmacro with-clsql-retry ((&key (max 10)) &body forms)
  "Keep trying to execute `forms' while they keep
signaling an `clsql:sql-condition' with a random retry of [0, 99ms]
up to `max' times (Default: 10)"
  (alexandria:with-gensyms (tries upper condition)
    `(let ((,upper ,max))
       (dotimes (,tries ,upper)
         (handler-case
             (return (progn ,@forms))
           (clsql:sql-condition (,condition)
             (unless (< (1+ ,tries) ,upper)
               (error ,condition))))))))

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
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'virtual-direct-slot-definition)
    (call-next-method)))

(defmethod initialize-instance :after ((slot virtual-direct-slot-definition) &rest initargs)
  ;; Eval the supplied "function" in case it's still a CONS
  (setf (virtual-slot-definition-function slot)
        (eval (virtual-slot-definition-function slot)))
  slot)

;;; Virtual effective slot definition
(defclass virtual-effective-slot-definition (clsql-sys::view-class-effective-slot-definition virtual-slot-definition)
  nil)

(defmethod effective-slot-definition-class ((class db-with-virtual-slots-class) &rest initargs)
  (let ((slot-alloc (getf initargs :allocation)))
    (if (eq slot-alloc :virtual)
        (progn
          (find-class 'virtual-effective-slot-definition))
      (call-next-method))))

(defmethod compute-effective-slot-definition ((class db-with-virtual-slots-class) name direct-slot-defs)
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-defs)
      (when (typep slotd 'virtual-direct-slot-definition)
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
        (setf (slot-value effective-slotd 'clsql-sys::db-kind) :virtual) ;; TODO: Hackity hack hack hack
        (return)))
    effective-slotd))

;; Access methods for virtual func access
(defmethod slot-value-using-class ((class db-with-virtual-slots-class) object slot)
  (if (typep slot 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slot)
               :get object)
    (call-next-method)))

(defmethod (setf slot-value-using-class) (value (class db-with-virtual-slots-class) object slot)
  (if (typep slot 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slot)
               :set object value)
    (call-next-method)))

(defmethod slot-boundp-using-class ((class db-with-virtual-slots-class) object slot)
  (if (typep slot 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slot)
                 :is-set object)
    (call-next-method)))

(defmethod slot-makunbound-using-class ((class db-with-virtual-slots-class) object slot)
  (if (typep slot 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slot)
                 :unset object)
    (call-next-method)))

(defmethod slot-exists-p-using-class ((class db-with-virtual-slots-class) object slot)
  (or (call-next-method)
      (and (find slot (class-slots class) :key 'slot-definition-name)
           t)))


;;; Database tweaks
(defmethod clsql-sys:database-get-type-specifier
    ((type (eql 'integer)) args database (db-type (eql :sqlite3)))
  (declare (ignore database db-type))
  (if args
      (format nil "INTEGER(~A)" (car args))
      "INTEGER"))

(defmethod clsql-sys::database-pkey-constraint ((class clsql-sys::standard-db-class)
						(database clsql-sqlite3::database)))

(defmethod clsql-sys::database-constraint-statement :around (constraint-list (database clsql-sqlite3::database))
  (flet ((filter-constraints (constraints
                              &optional (blacklist '(:auto-increment)))
           (remove-if #'(lambda (c) (member c blacklist)) constraints)))
    (let ((constraints (filter-constraints constraint-list)))
      (call-next-method constraints database))))

(defmethod clsql-sys::query :around ((query-expression string) &rest args &key database &allow-other-keys)
  "Fix the query to find the last inserted ID for sqlite3
TODO: May be no longer called anymore in the currently pinned version of CLSQL
see `clsql-sys:database-last-auto-increment-id' below"
  (let ((sql (typecase database
               (clsql-sqlite3::database
                (if (search "LAST_INSERT_ID()" query-expression)
                    (cl-ppcre:regex-replace "LAST_INSERT_ID\\(\\)" query-expression "LAST_INSERT_ROWID()")
                  query-expression))
               (otherwise query-expression))))
    (apply #'call-next-method `(,sql ,@args))))

(defmethod clsql-sys:database-last-auto-increment-id ((database clsql-sqlite3:sqlite3-database) table column)
  "Return the last inserted PK for an sqlite 3 database"
  (car (clsql:query (format nil "select last_insert_rowid() from ~A" (symbol-name table))
                    :flatp t :field-names nil)))
;;; Just helpers
(defun make-uuid4 (&optional (as :string))
  (let ((uuid (uuid:make-v4-uuid)))
    (ecase as
      (:string
       (format nil "~A" uuid))
      (:raw
       uuid))))

(defun update-template-from-instance (instance val)
  "Update strings replacing {name} with the value of slot named `name'"
  (let ((result val))
    (cl-ppcre:do-register-groups (slot) ("{([\\w_-]+)}" val)
      (let* ((slot-sym (intern (string-upcase slot) (symbol-package (type-of instance))))
             (exists (slot-exists-p instance slot-sym))
             (slot-val (and exists (slot-value instance slot-sym))))
        (setf result (cl-ppcre:regex-replace (format nil "{~A}" slot) val slot-val))))
    result))
