(in-package :api-app)

(defmethod find-forwarder ((agent api-agent) name)
  "Finds a forwarder by name"
  (assoc name (forwarders agent) :test #'string=))

(defmethod forwarder-with-handlers (forwarder (agent api-agent))
  "Adds handler sockets to the forwarder information nondestructively."
  (let* ((handlers (handlers agent))
         (forwarder-name (car forwarder))
         (routes (assoc :routes (cdr forwarder)))
         (result (remove routes (cdr forwarder)))
         ;; Append send and recv sockets to each route, where they
         ;; exist
         (new-routes (mapcar #'(lambda (route)
                                 (let* ((route-name (cdr (assoc :name route)))
                                        (path (cdr (assoc :route route)))
                                        (handler (cdr (find (handler-name forwarder-name route-name) handlers :key #'car :test #'string=)))
                                        (send (cdr (assoc :send handler)))
                                        (recv (cdr (assoc :recv handler))))
                                   (when (and send recv)
                                     (appendf route (list (cons :push send) (cons :sub recv))))))
                             (cdr routes))))
    ;; Rebuild the result list
    (append result (list (cons :routes new-routes)))))
