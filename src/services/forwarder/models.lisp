(in-package :fdog-forwarder)

(clsql:def-view-class fdog-forwarder-hostpath ()
  ((id :type integer
       :db-constraints (:primary-key :auto-increment)
       :db-kind :key
       :reader fdog-forwarder-host-id)
   (forwarder-id :type integer
                 :reader fdog-forwarder-host-forwarder-id)
   (host :type string
         :initarg :host
         :initform nil)
   (path :type string
         :initarg :path
         :initform "/"
         :accessor fdog-forwarder-path))
  (:base-table fdog-forwarder-hostpath
   :documentation "A host and path pair for a given forwarder."))

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
              :initarg :listen-on
              :initform 9910)
   (forward-to :type integer
               :initarg :forward-to
               :initform 9999)

   (hostpaths :db-kind :join
              :accessor fdog-forwarder-hostpaths
              :db-info (:join-class fdog-forwarder-hostpath
                        :home-key id
                        :foreign-key forwarder-id
                        :set t)))
  (:base-table fdog-forwarder
   :documentation "Database model describing a forwarder endpoint."))

;; Model methods
(defmethod find-forwarder (&rest keys &key name one)
  #.(clsql:locally-enable-sql-reader-syntax)
  (cond (one
         (car (apply #'find-forwarder (progn (setf (getf keys :one) nil)
                                             keys))))
        (name
         (clsql:select 'fdog-forwarder :flatp t :refresh t
                       :where [= [slot-value 'fdog-forwarder 'name] name]))
        (t
         (clsql:select 'fdog-forwarder :flatp t :refresh t)))
  #.(clsql:restore-sql-reader-syntax-state))


;; API hooks
#.(clsql:locally-enable-sql-reader-syntax)

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
                                              :forward-to (next-forwarder-port)
                                              :listen-on (next-forwarder-port))))
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
                          (:sub . ,(format nil "tcp://~A:~A" (fdog:get-local-address) listen-on))
                          (:push . ,(format nil "tcp://~A:~A" (fdog:get-local-address) forward-to)))
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

