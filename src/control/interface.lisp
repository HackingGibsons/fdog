(in-package :fdog-control)

(defclass fdog-interface ()
  ((server :initarg :server
           :initform "control"
           :accessor fdog-interface-server)
   (bridges :initform ()
            :accessor fdog-interface-bridges)
   (routes :initform ()
           :accessor fdog-interface-routes))
  (:documentation "An interface for interacting with fdog through Mongrel2"))

(defmethod interface-bridge-matching ((self fdog-interface) route-path-regex)
  "Find the bridge configured for the route with a path matching `route-path-regex'"
  nil)

(defmethod interface-stop ((self fdog-interface))
  (interface-start-server self)
  (interface-stop-server self))

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
  (with-slots (server) self
    (unless (typep server 'mongrel2-server)
      (let ((servers (fdog-m2sh:servers :name server :refresh t)))
        (if servers
            (setf server (car servers))
            (error (format nil "Could not initialize server from value: ~A" server))))))
  (initialize-interface self))

(defmethod initialize-interface ((self fdog-interface))
  (with-slots (server bridges routes) self
    (mapcar #'request-handler-stop bridges)
    (setf bridges ()
          routes ())

    (dolist (route (mongrel2-host-routes (mongrel2-server-default-host server)) self)
      (pushnew route routes)
      (with-accessors ((target mongrel2-route-target)) route
        (typecase target
          (mongrel2-handler
           (pushnew (configure-bridges-for target) bridges))
          (otherwise
           (unless target
             (log-for (warn) "NIL TARGET FOR ROUTE ~A(~A)" route (slot-value route 'fdog-models::id)))))))))
