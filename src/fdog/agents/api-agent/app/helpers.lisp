(in-package :api-app)

(defmethod find-forwarder ((agent api-agent) name)
  "Finds a forwarder by name"
  (assoc name (forwarders agent) :test #'string=))

(defmethod forwarder-with-handlers (forwarder (agent api-agent))
  "Adds handler sockets to the forwarder information nondestructively."
  (let* ((endpoints (endpoints agent))
         (forwarder-name (car forwarder))
         (routes (assoc :routes (cdr forwarder)))
         (result (remove routes (cdr forwarder)))
         ;; Append send and recv sockets to each route, where they
         ;; exist
         (new-routes (mapcar #'(lambda (route)
                                 (let ((route-name (cdr (assoc :name route)))
                                        (path (cdr (assoc :route route))))
                                   (when-bind endpoint (find-if #'(lambda (e) (and
                                                                          (string= forwarder-name (cdr (assoc :forwarder e)))
                                                                          (string= route-name (cdr (assoc :route e)))
                                                                          (string= path (cdr (assoc :path e)))))
                                                                endpoints)
                                     (appendf route (cons :endpoints
                                                          (mapcar #'(lambda (e)
                                                                      (let* ((info (cdr e))
                                                                             (push (cdr (assoc :push info)))
                                                                             (sub (cdr (assoc :sub info))))
                                                                        (list (cons :push push) (cons :sub sub)))) (cdr (assoc :endpoints endpoint))))))))
                             (cdr routes))))
    ;; Rebuild the result list
    (append result (list (cons :routes new-routes)))))
