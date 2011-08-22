(in-package :fdog-forwarder)

;; Engine creation
(defmethod make-alias-endpoint ((engine forwarder-engine) (alias fdog-forwarder-alias))
  (let* ((forwarder (fdog-forwarder-alias-forwarder alias))
         (queue-p (forwarder-queuing-p forwarder)))
    (if queue-p
        (prog1 (make-instance 'forwarder-queue-endpoint :engine engine :alias alias)
          (log-for (trace) "Making queued alias endpoint for ~A of ~A" alias forwarder))
        (prog1 (make-instance 'forwarder-engine-endpoint :engine engine :alias alias)
          (log-for (trace) "Making plain endpoint for ~A of ~A" alias forwarder)))))


(defmethod make-forwarder-engine ((forwarder fdog-forwarder) &key servers)
  "Construct a forwarder-engine class from an fdog-forwarder model `forwarder'"
  (log-for (trace) "Building forwarder engine from ~A" forwarder)
  (let ((engine (make-instance 'forwarder-engine :forwarder forwarder :servers servers))
        (queue-p (forwarder-queuing-p forwarder)))
    (setf (forwarder-engine-endpoint engine)
          (if queue-p
              (prog1 (make-instance 'forwarder-queue-endpoint :engine engine)
                (log-for (trace) "Making queued endpoint for ~A" forwarder))
              (prog1 (make-instance 'forwarder-engine-endpoint :engine engine)
                (log-for (trace) "Making plain endpoint for ~A" forwarder)))

          (forwarder-engine-alias-endpoints engine)
          (mapcar #'(lambda (alias) (make-alias-endpoint engine alias))
                  (fdog-forwarder-aliases forwarder)))
    engine))

(defmethod initialize-instance :after ((engine forwarder-engine) &rest initargs)
  (declare (ignorable initargs))
  (log-for (trace) "Initializing bridges for engine: ~A" engine)
  (with-slots (forwarder aliases servers bridges) engine
    ;; Build multibridges for all unique paths
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
    (remove-if-not #'multibridge-running-p (forwarder-engine-bridges engine))))

(defgeneric forwarder-engine-start (engine)
  (:documentation "Start forwarder engine `engine' so it begins to serve requests.")
  (:method ((engine (eql :all)))
    (mapc #'forwarder-engine-start *forwarders*)))

(defgeneric forwarder-engine-stop (engine)
  (:documentation "Stop forwarder engine `engine' so it ceases to serve requests.")
  (:method ((engine (eql :all)))
    (mapc #'forwarder-engine-stop *forwarders*)))

(defmethod forwarder-engine-start ((engine forwarder-engine))
  (log-for (trace) "Starting engine: ~A" engine)
  (engine-endpoint-start (forwarder-engine-endpoint engine))
  (log-for (trace) "Starting alias endpoints.")
  (mapcar #'engine-endpoint-start (forwarder-engine-alias-endpoints engine))
  (log-for (trace) "Starting main endpoint.")
  (mapcar #'multibridge-start (forwarder-engine-bridges engine)))

(defmethod forwarder-engine-stop ((engine forwarder-engine))
  (log-for (trace) "Shutting down engine: ~A" engine)
  (mapcar #'multibridge-stop (forwarder-engine-bridges engine))
  (engine-endpoint-stop (forwarder-engine-endpoint engine)))

(defmethod forwarder-engine-handlers ((engine forwarder-engine))
  (mapcar #'multibridge-handler (forwarder-engine-bridges engine)))


;; Data access helpers
(defmethod fdog-forwarder-name ((engine forwarder-engine))
  (fdog-forwarder-name (forwarder-engine-forwarder engine)))
