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
   (host :type string
         :initarg :host
         :initform nil)
   (path :type string
         :initarg :path
         :initform "/")
   (listen-on :type string
              :initarg :listen-on
              :initform "tcp://127.0.0.1:9910")
   (forward-to :type string
               :initarg :forward-to
               :initform "tcp://127.0.0.1:9999"))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

(defmethod print-object ((self fdog-forwarder) stream)
  (with-slots (id host path listen-on forward-to) self
    (format stream "#<DBForwarder(~A): ~A~A ~A => ~A>"
            id host path listen-on forward-to)))

;; Model methods
(defmethod find-forwarder (&rest keys &key name host path one)
  #.(clsql:locally-enable-sql-reader-syntax)
  (cond (one
         (car (apply #'find-forwarder (progn (setf (getf keys :one) nil)
                                             keys))))
        ((not (or name host path))
         (clsql:select 'fdog-forwarder :flatp t :refresh t))
        ((and name (not (or host path)))
         (clsql:select 'fdog-forwarder :flatp t :refresh t
                       :where [= [slot-value 'fdog-forwarder 'name] name]))
        (t
         (error "TODO: :(")))
  #.(clsql:restore-sql-reader-syntax-state))


;; API hooks
#.(clsql:locally-enable-sql-reader-syntax)

(defmethod make-forward-to-address (&rest rest &key &allow-other-keys)
  (declare (ignorable rest))
  (format nil "tcp://127.0.0.1:~A" (next-forwarder-port)))

(defmethod make-listen-on-address (&rest rest &key &allow-other-keys)
  (declare (ignorable rest))
  (format nil "tcp://127.0.0.1:~A" (next-forwarder-port)))

(defmethod api/endpoint ((m (eql :post)) (p (eql :|/forwarders/create/|))
                         handler request raw)
  (let* ((spec (json:decode-json-from-string (m2cl:request-body request)))
         (name (cdr (assoc :name spec)))
         (host (cdr (assoc :host spec))))
    (log-for (trace) "Spec: ~A" spec)
    (log-for (trace) "Name: ~A" name)
    (log-for (trace) "Host: ~A" host)
    (if (find-forwarder :name name :one t)
        (log-for (trace) "Already exists.")
        (progn
          (log-for (trace) "Creating")
          (let ((new-forwarder (make-instance 'fdog-forwarder :name name :host host
                                              :forward-to (make-forward-to-address)
                                              :listen-on (make-listen-on-address))))
            (clsql:update-records-from-instance new-forwarder)
            (log-for (trace) "Created new forwarder: ~A" new-forwarder)
            (describe new-forwarder))))))


(defmethod api/endpoint-with-args ((m (eql :get)) (p (eql :|/forwarders|)) rest
                                   handler request raw)
  (ppcre:register-groups-bind (forwarder) ("^/?(.*?)/?$" rest)
    (setf forwarder (find-forwarder :name forwarder :one t))
    (unless forwarder
      (error '404-condition :data (format nil "Forwarder ~A not found" rest)))

    (with-chunked-stream-reply (handler request stream
                                :headers ((header-json-type)))
      (with-slots (name host path listen-on forward-to) forwarder
      (json:encode-json `((:name . ,name)
                          (:host . ,host)
                          (:sub . ,listen-on)
                          (:push . ,forward-to))
                        stream)))))

(defmethod api/endpoint ((m (eql :get)) (p (eql :|/forwarders/|)) handler request raw)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json `(,@(mapcar #'(lambda (forwarder)
                                     (with-slots (name host path) forwarder
                                       `(,name . ((:host . ,host)
                                                  (:path . ,path)))))
                                 (find-forwarder)))
                      stream)))

;; //EOAPI Hooks
#.(clsql:restore-sql-reader-syntax-state)

