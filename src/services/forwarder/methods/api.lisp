(in-package :fdog-forwarder)

;; API hooks
#.(clsql:locally-enable-sql-reader-syntax)

(defmethod api/forwarder/root (handler request forwarder args)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
      (with-slots (name listen-on forward-to) forwarder
        (json:encode-json `((:name . ,name)
                            (:sub . ,(format nil "tcp://~A:~A" (fdog:get-local-address) listen-on))
                            (:push . ,(format nil "tcp://~A:~A" (fdog:get-local-address) forward-to)))
                          stream))))

(defmethod api/forwarder/make-route (handler request forwarder args)
  (log-for (trace) "Adding a route to a handler.")
  (let* ((spec (json:decode-json-from-string (m2cl:request-body request)))
         added)
    (log-for (trace) "Spec: [~A]" spec)
    (destructuring-bind (type &rest spec) (car spec)
      (log-for (trace) "Despec: [~A]::[~A]" type spec)
      (case type
        (:exact
         (log-for (trace) "Adding exact: ~A" spec)
         (setf added
               (make-forwarder-hostpath forwarder
                                        (cdr (assoc :host spec))
                                        (cdr (assoc :path spec)))))))
    (with-chunked-stream-reply (handler request stream
                                        :headers ((header-json-type)))
      (json:encode-json `((:ok . ((,(fdog-hostpath-host added) .
                                  ,(fdog-hostpath-path added)))))
                        stream))))


(defmethod api/forwarder/404 (handler request forwarder args)
  (error '404-condition
         :data (format nil "No routes matching ~A for a forwarder" args)))

;; API Roots
;; These get requests dispatched to them and pass control to more specific routes
(defmethod api/endpoint-with-args ((m (eql :get)) (p (eql :|/forwarders|)) rest
                                   handler request raw)
  (ppcre:register-groups-bind (forwarder rest) ("^/?([^/]+)(/?.*$)" rest)
    (setf forwarder (find-forwarder :name forwarder :one t))
    (unless forwarder
      (error '404-condition :data (format nil "Forwarder ~A not found" rest)))

    (with-dispatch-on rest &route
       (funcall &route handler request forwarder rest)

       (:exact "/" :responder 'api/forwarder/root)
       (:404 :responder 'api/forwarder/404))))

(defmethod api/endpoint-with-args ((m (eql :post)) (p (eql :|/forwarders|)) rest
                                   handler request raw)
  (ppcre:register-groups-bind (forwarder rest) ("^/?([^/]+)(/?.*$)" rest)
    (setf forwarder (find-forwarder :name forwarder :one t))
    (unless forwarder
      (error '404-condition :data (format nil "Forwarder ~A not found" rest)))

    (with-dispatch-on rest &route
       (funcall &route handler request forwarder rest)

       (:exact "/make-route/" :responder 'api/forwarder/make-route)
       (:404 :responder 'api/forwarder/404))))

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

;; Non-dispatching roots
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

;; //EOAPI Hooks
#.(clsql:restore-sql-reader-syntax-state)

