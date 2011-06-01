(in-package :fdog-control)

(defclass fdog-interface ()
  ((server :initarg :server
           :initform "control"
           :accessor fdog-interface-server)
   (host :initarg :host
         :accessor fdog-interface-host)
   (bridges :initform ()
            :accessor fdog-interface-bridges)
   (routes :initform ()
           :accessor fdog-interface-routes))
  (:documentation "An interface for interacting with fdog through Mongrel2"))

(defmethod interface-bridge-matching ((self fdog-interface) route-path &optional regex)
  "Find the bridge configured for the route with a path matching `route-path-regex'.
All bridges found are returned as the second value, in case of multiple results.
Only the first is returned as the first value"
  (flet ((matching-bridge-p (bridge)
           (let* ((handler (handler-bridge-db-handler bridge))
                  (route (mongrel2-target-route handler)))
             (with-accessors ((path mongrel2-route-path)) route
               (if regex
                   (ppcre:scan route-path path)
                   (string= route-path path))))))
    (let ((bridges (with-slots (bridges) self
                     (remove-if-not #'matching-bridge-p bridges))))
      (values (car bridges)
              bridges))))

(defmacro interface-configure-bridges ((interface) &body configs)
  "Helper for calling a specific configuration function with a bridge
for a route you expect to exist with a given path, e.g.:

 (defun configure-default-response (bridge)
   (request-handler-add-string-responder bridge (lambda (r) \"Hello world\")))

 (let ((interface (make-instance 'fdog-interface ....)))
    (interface-configure-bridges (interface)
     \"/\" :mount-bridge 'configure-default-response))

Will configure a Hello World responder for the bridge mounted on a route
with an exact pathname of / on the server configured for `interface'"
  `(progn
     ,@(loop for config in configs collecting
            (destructuring-bind (path &rest spec) config
              `(let* ((path ,path) (spec ',spec) (interface ,interface)
                      (bridge (interface-bridge-matching interface path)))
                 (log-for (trace) "Configuration: ~A => ~A" path spec)
                 (log-for (trace) "Bridge: ~A" bridge)
                 (if bridge
                     (funcall (eval (getf spec :mount-bridge))
                              bridge interface)
                     (error (format nil "No bridge found for route ~A" path))))))))

(defmethod interface-stop ((self fdog-interface))
  (interface-stop-bridges self)
  (interface-stop-server self))

(defmethod interface-start ((self fdog-interface))
  (interface-start-bridges self)
  (interface-start-server self))

(defmethod interface-start-server ((self fdog-interface))
  (with-slots (server) self
    (unless (mongrel2-server-running-p server)
      (interface-stop-server self)
    (mongrel2-server-signal/block server :start))))

(defmethod interface-stop-server ((self fdog-interface))
  (with-slots (server) self
    (mongrel2-server-signal/block server :stop)))

(defmethod interface-stop-bridges ((self fdog-interface))
  (with-slots (bridges) self
    (mapcar #'request-handler-stop bridges)))

(defmethod interface-start-bridges ((self fdog-interface))
  (with-slots (bridges) self
    (interface-stop-bridges self)
    (mapcar #'request-handler-start bridges)))

(defmethod initialize-instance :after ((self fdog-interface) &rest initargs)
  (declare (ignore initargs))
  (with-slots (server host) self
    (unless (typep server 'mongrel2-server)
      (let ((servers (fdog-m2sh:servers :name server :refresh t)))
        (if servers
            (setf server (car servers))
            (error (format nil "Could not initialize server from value: ~A" server)))))

    (unless (slot-boundp self 'host)
      (setf host (mongrel2-server-default-host server)))
    (unless (typep host 'mongrel2-host)
      (setf host
            (find host (fdog-m2sh:server-hosts server :refresh t)
                  :key #'mongrel2-host-name :test #'string=)))
    (unless (and host (member (model-pk host) (fdog-m2sh:server-hosts server :refresh t)
                              :test #'= :key #'model-pk))
      (log-for (dribble) "Configured host: ~A" (mongrel2-host-name host))
      (log-for (dribble) "Server: ~A" (mongrel2-server-name server))
      (log-for (dribble) "Hosts: ~A" (mapcar #'mongrel2-host-name (fdog-m2sh:server-hosts server :refresh t)))
      (error (format nil "Configured host ~A not found or not in hosts of ~A" host server))))
  (initialize-interface self))

(defmethod initialize-interface ((self fdog-interface))
  (with-slots (server bridges routes) self
    (mapcar #'request-handler-stop bridges)
    (setf bridges ()
          routes ())

    (dolist (route (mongrel2-host-routes (fdog-interface-host self)) self)
      (pushnew route routes)
      (with-accessors ((target mongrel2-route-target)) route
        (typecase target
          (mongrel2-handler
           (pushnew (configure-bridges-for target) bridges))
          (otherwise
           (unless target
             (log-for (warn) "NIL TARGET FOR ROUTE ~A(~A)" route (slot-value route 'fdog-models::id)))))))))
