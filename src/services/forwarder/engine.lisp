(in-package :fdog-forwarder)

;; Forwarder engine, that which makes the definitions dance
(defclass forwarder-engine ()
  ((forwarder :initarg :forwarder
              :accessor forwarder-engine-forwarder)
   (servers :initarg :servers
            :initform (forwarder-servers)
            :accessor forwarder-engine-servers)
   (bridges :initarg :bridges
            :initform ()
            :accessor forwarder-engine-bridges))
  (:documentation "The engine that manages the forwarding of requests for an endpoint"))

(defclass multibridge ()
  ((engine :initarg :engine
           :accessor multibridge-engine)
   (handler :initarg :handler
            :initform nil
            :accessor multibridge-handler)
   (path :initarg :path
         :initform "/"
         :accessor multibridge-path)
   (bridges :initform ()
            :accessor multibridge-briges)))

(defmethod print-object ((multibridge multibridge) s)
  (with-slots (handler path bridges) multibridge
    (format s "#<Multibridge: ~A/~A Path: ~A Handler-ident: ~A>"
            (length (remove-if-not #'request-handler-running-p bridges)) (length bridges)
            path (mongrel2-handler-send-ident handler))))

;; Engine creation
(defmethod make-multibridge ((engine forwarder-engine) (handler mongrel2-handler))
  (log-for (trace) "Building multibridge for: ~A" handler)
  (make-instance 'multibridge :engine engine :handler handler
                 :path (mongrel2-route-path (mongrel2-target-route handler))))

(defmethod make-forwarder-engine ((forwarder fdog-forwarder) &key servers)
  "Construct a forwarder-engine class from an fdog-forwarder model `forwarder'"
  (log-for (trace) "Building forwarder engine from ~A" forwarder)
  (let ((engine (make-instance 'forwarder-engine :forwarder forwarder :servers servers)))
    engine))

(defmethod initialize-instance :after ((engine forwarder-engine) &rest initargs)
  (declare (ignore initargs))
  (log-for (trace) "Initializing bridges for engine: ~A" engine)
  (with-slots (forwarder servers bridges) engine
    (let ((paths (forwarder-uniqe-paths forwarder)))
      (log-for (trace) "Forwarder has paths: ~A" paths)
      (dolist (path paths bridges)
        (dolist (server servers)
          (push (make-multibridge engine
                                  (find-mongrel2-handler
                                   :send-ident (send-ident-for forwarder path (mongrel2-server-ssl-p server))))
                bridges))))))


;; Engine state control
(defgeneric forwarder-engine-running-p (engine)
  (:documentation "Boolean representation of engine operation")
  (:method ((engine forwarder-engine))
    (log-for (warn) "TODO: Not implemented")
    nil))

(defgeneric forwarder-engine-start (engine)
  (:documentation "Start forwarder engine `engine' so it begins to serve requests.")
  (:method ((engine (eql :all)))
    (mapc #'forwarder-engine-start
          (remove-if #'forwarder-engine-running-p *forwarders*))))

(defgeneric forwarder-engine-stop (engine)
  (:documentation "Stop forwarder engine `engine' so it ceases to serve requests.")
  (:method ((engine (eql :all)))
    (mapc #'forwarder-engine-stop
          (remove-if-not #'forwarder-engine-running-p *forwarders*))))

(defmethod forwarder-engine-start ((engine forwarder-engine))
  (log-for (trace) "Starting engine: ~A" engine)
  :undef)

(defmethod forwarder-engine-stop ((engine forwarder-engine))
  (log-for (trace) "Shutting down engine: ~A" engine)
  :undef)
