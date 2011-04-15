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
        (setf (slot-value effective-slotd 'clsql-sys::db-kind) :virtual) ;; TODO: Hackity hack hack hack
        (return)))
    (format t "Computed: ~A~%" effective-slotd)
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

