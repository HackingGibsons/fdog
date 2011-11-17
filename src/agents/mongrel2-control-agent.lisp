(in-package :mongrel2-agent)
;; Load the sqlite3 DB
(clsql:initialize-database-type :database-type :sqlite3)

(defcategory mongrel2-state-machine)

(defclass mongrel2-control-agent (standard-hypervisor-agent)
  ())

(defclass mongrel2-state-machine (standard-state-machine)
  ((behavior :initform nil :initarg behavior :reader mongrel2-state-machine-behavior))
  (:documentation "State machine to search for mongrel2 servers to supervise."))

(defgeneric search-mongrels (machine)
  (:method (machine)
    "The default method is a no-op.")
  (:documentation "Search for a mongrel2 server."))

(defstate mongrel2-state-machine :search (info)
  (let* ((event (getf info :event)))
    (cond ((eql event :boot)
           (search-mongrels  machine))
          (t
           (values :saw :fire-again)))))

(defstate mongrel2-state-machine :saw (info)
  :found)

(defstate mongrel2-state-machine :found (info)
  :link)

(defstate mongrel2-state-machine :link (info)
  :done)

(defstate mongrel2-state-machine :done (info))

(defclass mongrel2-state-machine-mixin ()
  ((mongrel2-state-machine :initarg :state-machine :initform nil :accessor mongrel2-state-machine)))

(defmethod initialize-instance :after ((behavior mongrel2-state-machine-mixin) &key)
  (setf (mongrel2-state-machine behavior)
        (make-instance 'mongrel2-state-machine :behavior behavior :state :search)))

(defbehavior find-mongrels (:on (:saw :directory :from :eye) :include (mongrel2-state-machine-mixin) :do :invoke-with-event) (organ event)
  (funcall (mongrel2-state-machine behavior) event))

(defmethod agent-special-event :after ((agent mongrel2-control-agent) (head (eql :boot)) event)
  (make-find-mongrels (find-organ agent :head)))





