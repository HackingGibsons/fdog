(in-package :fdog-forwarder)

;; API hooks
#.(clsql:locally-enable-sql-reader-syntax)

(defmethod api/endpoint ((m (eql :post)) (p (eql :|/forwarders/create/|))
                         handler request raw)
  (let* ((spec (json:decode-json-from-string (m2cl:request-body request)))
         (name (cdr (assoc :name spec)))
         (hostpaths (cdr (assoc :hosts spec)))
         (existing (find-forwarder :name name :one t))
         hosts)
    (if (and (not existing) hostpaths)
        (with-chunked-stream-reply (handler request stream
                                            :headers ((header-json-type)))
          (dolist (hostpath hostpaths hosts)
            (destructuring-bind (host . path) hostpath
              (setf host
                    (string-downcase
                     (typecase host (symbol (symbol-name host)) (t host))))
              (push (cons host path) hosts)))
          (log-for (trace) "Making forwarder: Name: ~A Hosts: ~A" name hosts)
          (apply #'make-forwarder `(,name ,@hosts))
          (init-forwarders)
          (json:encode-json spec stream))
        (error '404-condition
               :data (json:encode-json-to-string '((:error . "Could not create")))))))

(defmethod api/endpoint-with-args ((m (eql :get)) (p (eql :|/forwarders|)) rest
                                   handler request raw)
  (ppcre:register-groups-bind (forwarder rest) ("^/?([^/]+)(/?.*$)" rest)
    (setf forwarder (find-forwarder :name forwarder :one t))
    (unless forwarder
      (error '404-condition :data (format nil "Forwarder ~A not found" rest)))

    (with-chunked-stream-reply (handler request stream
                                :headers ((header-json-type)))
      (with-slots (name host path listen-on forward-to) forwarder
      (json:encode-json `((:name . ,name)
                          (:sub . ,(format nil "tcp://~A:~A" (fdog:get-local-address) listen-on))
                          (:push . ,(format nil "tcp://~A:~A" (fdog:get-local-address) forward-to)))
                        stream)))))

(defmethod api/endpoint ((m (eql :get)) (p (eql :|/forwarders/|)) handler request raw)
  (flet ((forwarder->structure (forwarder)
           "Format a forwarder to a structure in the form:
 (name . ((host . path) ... (host_n . path_n)))"
           `(,(fdog-forwarder-name forwarder) .
              ,(mapcar #'(lambda (hostpath)
                          `(,(fdog-hostpath-host hostpath) . ,(fdog-hostpath-path hostpath)))
                      (fdog-forwarder-hostpaths forwarder)))))
    (with-chunked-stream-reply (handler request stream
                                :headers ((header-json-type)))
      (let ((forwarder-structure (mapcar #'forwarder->structure (find-forwarder))))
        (log-for (dribble) "Constructed structure: ~A" forwarder-structure)
        (json:encode-json forwarder-structure stream)))))

;; //EOAPI Hooks
#.(clsql:restore-sql-reader-syntax-state)

