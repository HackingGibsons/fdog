(in-package :fdog-forwarder)

;; API hooks
#.(clsql:locally-enable-sql-reader-syntax)

(defmethod api/forwarder/metrics (handler request forwarder args)
  "Cloudkick-compatible metrics for forwarders"
  (log-for (trace) "Fetching forwarder metrics for: ~A" forwarder)
  (with-slots (name) forwarder
    (with-chunked-stream-reply (handler request stream :headers ((header-json-type)))
      (json:encode-json `((:state . "ok")
                          (:metrics .
                           (((:type . "int") (:name . "queue_length") (:value . ,(request-queue-length forwarder)))
                            ((:type . "gauge") (:name . "request_throughput") (:value . ,(request-count name)))
                            ((:type . "gauge") (:name . "response_throughput") (:value . ,(response-count name)))))) stream))))

(defmethod api/forwarder/update (handler request forwarder args)
  (let* ((spec (json:decode-json-from-string (m2cl:request-body request)))
         (enabled (forwarder-queuing-p forwarder)))
    (log-for (trace) "Update spec: ~A" spec)
    (if (cdr (assoc :queue spec))
        (forwarder-queue-enable forwarder)
        (forwarder-queue-disable forwarder))
    (unless (eq enabled (forwarder-queuing-p forwarder))
      (log-for (trace) "Queue state of forwarder changed, re-initing")
      (init-forwarders))
    (api/forwarder/root handler request forwarder args)))

(defmethod api/forwarder/root (handler request forwarder args)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
      (with-slots (name listen-on forward-to) forwarder
        (json:encode-json `((:name . ,name)
                            (:queue . ,(forwarder-queuing-p forwarder))
                            (:sub . ,(format nil "tcp://~A:~A" (fdog:get-local-address) listen-on))
                            (:push . ,(format nil "tcp://~A:~A" (fdog:get-local-address) forward-to)))
                          stream))))

(defmethod api/forwarder/aliases/index (handler request forwarder args)
  (log-for (trace) "Forwarder alias index for: ~A" forwarder)
  (with-chunked-stream-reply (handler request stream
                              :headers ((header-json-type)))
    (json:encode-json (mapcar #'fdog-forwarder-alias-name
                              (fdog-forwarder-aliases forwarder))
                      stream)))

(defmethod api/forwarder/alias/create (handler request forwarder args)
  (log-for (trace) "Forwarder alias creation for: ~A" forwarder)
  (log-for (trace) "Body: ~A" (m2cl:request-body request))
  (let* ((spec (json:decode-json-from-string (m2cl:request-body request)))
         (name (cdr (assoc :name spec)))
         (method (cdr (assoc :method spec)))
         (match (cdr (assoc :match spec))))

    (unless (and spec name (or method match))
      (log-for (trace) "Invalid spec: ~A" spec)
      (error 'fdog-control:400-condition :data (format nil "The must contain: (and name (or method match)) to be valid.")))

    (when (find-forwarder-alias forwarder name)
      (log-for (trace) "Trying to build forwarder with known name: ~A" name)
      (error 'fdog-control:400-condition :data (format nil "An alias by that name already exists.")))

    (let ((alias (make-forwarder-alias forwarder name :method method :match match)))
      (log-for (trace) "Built alias: ~A" alias)
      (init-forwarders)
      (log-for (trace) "Re-inited.")
      (with-chunked-stream-reply (handler request stream :headers ((header-json-type)))
        (json:encode-json spec stream)))))

(defmethod api/forwarder/alias (handler request forwarder args)
  (let* ((alias-name (ppcre:regex-replace "^/aliases/([\\w_-]+)/" args "\\1"))
         (alias (find-forwarder-alias forwarder alias-name)))
    (log-for (trace) "Requesting forwarder alias: ~A => ~A => ~A" forwarder alias-name alias)

    (unless alias
      (error 'fdog-control:404-condition :data (format nil "No aliases named [~A] for forwarder [~A]"
                                          (fdog-forwarder-name forwarder) alias-name)))

    (with-chunked-stream-reply (handler request stream
                                :headers ((header-json-type)))
      (with-slots (name listen-on forward-to) alias
        (json:encode-json `((:name . ,name)
                            (:parent . ,(fdog-forwarder-name forwarder))
                            (:queue . ,(forwarder-queuing-p forwarder))
                            (:sub . ,(make-local-endpoint :port listen-on))
                            (:push . ,(make-local-endpoint :port forward-to)))
                          stream)))))

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
    (init-forwarders)
    (with-chunked-stream-reply (handler request stream
                                        :headers ((header-json-type)))
      (json:encode-json `((:ok . ((,(fdog-hostpath-host added) .
                                  ,(fdog-hostpath-path added)))))
                        stream))))


(defmethod api/forwarder/404 (handler request forwarder args)
  (error 'fdog-control:404-condition
         :data (format nil "No routes matching ~A for a forwarder" args)))

;; API Roots
;; These get requests dispatched to them and pass control to more specific routes
(defmethod api/forwarder/aliases/route (handler request forwarder args)
  (log-for (trace) "Routing an alias route for ~A => ~A" forwarder args)
  (with-dispatch-on args &route
      (funcall &route handler request forwarder args)

    (:exact "/aliases/" :responder 'api/forwarder/aliases/index)
    (:regex "/aliases/[\\w_-]+/" :responder 'api/forwarder/alias)

    (:404 :responder 'api/forwarder/404)))


(defmethod api/endpoint-with-args ((m (eql :get)) (p (eql :|/forwarders|)) rest
                                   handler request raw)
  (ppcre:register-groups-bind (forwarder rest) ("^/?([^/]+)(/?.*$)" rest)
    (setf forwarder (find-forwarder :name forwarder :one t))
    (unless forwarder
      (error 'fdog-control:404-condition))

    (with-dispatch-on rest &route
       (funcall &route handler request forwarder rest)

       (:regex "/aliases/.*"  :responder 'api/forwarder/aliases/route)
       (:exact "/metrics/" :responder 'api/forwarder/metrics)
       (:exact "/" :responder 'api/forwarder/root)
       (:404 :responder 'api/forwarder/404))))

(defmethod api/endpoint-with-args ((m (eql :post)) (p (eql :|/forwarders|)) rest
                                   handler request raw)
  (ppcre:register-groups-bind (forwarder rest) ("^/?([^/]+)(/?.*$)" rest)
    (setf forwarder (find-forwarder :name forwarder :one t))
    (unless forwarder
      (error 'fdog-control:404-condition))

    (with-dispatch-on rest &route
       (funcall &route handler request forwarder rest)

       (:exact "/make-route/" :responder 'api/forwarder/make-route)
       (:exact "/aliases/create/" :responder 'api/forwarder/alias/create)
       (:exact "/" :responder 'api/forwarder/update)
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
        (error 'fdog-control:400-condition
               :data (cond
                          (existing "Forwarder already exists")
                          ((not hostpaths) "Forwarder requires hostpaths"))))))

;; //EOAPI Hooks
#.(clsql:restore-sql-reader-syntax-state)

