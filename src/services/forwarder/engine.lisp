(in-package :fdog-forwarder)

;; Forwarder endpoint, a proxy between the handlers and an outside consumer
(defclass forwarder-engine-endpoint ()
  ((engine :initarg :engine
           :accessor endpoint-engine)

   (processor :initarg :processor
              :initform nil
              :accessor endpoint-processor)

   (context-threads :initargs :threads :initform 4
                    :accessor endpoint-context-threads)
   (context :initarg :context
            :initform nil
            :accessor endpoint-context)

   ;; Proxies
   (request-proxy-sock-addr :initarg :proxy-addr
                            :accessor endpoint-proxy-addr)
   (request-proxy-sock :initarg :proxy-sock
                       :accessor endpoint-proxy-sock)
   (response-process-sock :initarg :response-process-sock
                          :accessor endpoint-response-process-sock)

   ;; Public facing sockets
   (request-sock :initarg :push-sock
                 :accessor endpoint-request-sock)
   (response-sock :initarg :sub-sock
                  :accessor endpoint-response-sock)))

;; Multibridge, interface for running multiple bridges at the same time
;; TODO: Factor out engine subclass of multibridge
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
            :accessor multibridge-bridges)))

;; Forwarder engine, that which makes the definitions dance
(defclass forwarder-engine ()
  ((forwarder :initarg :forwarder
              :accessor forwarder-engine-forwarder)
   (servers :initarg :servers
            :initform (forwarder-servers)
            :accessor forwarder-engine-servers)
   (bridges :initarg :bridges
            :initform ()
            :accessor forwarder-engine-bridges)
   (endpoint :accessor forwarder-engine-endpoint))
  (:documentation "The engine that manages the forwarding of requests for an endpoint"))

(defmethod print-object ((object multibridge) s)
  (with-slots (handler path bridges) object
    (format s "#<Multibridge: ~A/~A Path: ~A Handler-ident: ~A>"
            (length (multibridge-running-bridges object)) (length bridges)
            path (mongrel2-handler-send-ident handler))))

;; Forwarder endpoint creation
(defmethod terminate-context ((endpoint forwarder-engine-endpoint))
  (when (endpoint-context endpoint)
    (log-for (trace) "Terminating endpoint context: ~A" endpoint)
    (zmq:term (endpoint-context endpoint))
    (setf (endpoint-context endpoint) nil)
    (log-for (trace) "Terminated: ~A" endpoint)))

(defmethod terminate-sockets ((endpoint forwarder-engine-endpoint))
  (log-for (warn) "TODO: terminate-sockets is a stub.")
  (values nil
          :undef))

(defmethod init-context ((endpoint forwarder-engine-endpoint))
  (terminate-context endpoint)
  (log-for (trace) "Creating ZMQ context for endpoint with ~A threads" (endpoint-context-threads endpoint))
  (setf (endpoint-context endpoint)
        (zmq:init (endpoint-context-threads endpoint))))

(defmethod init-sockets ((endpoint forwarder-engine-endpoint))
  (log-for (warn) "TODO: init-sockets is a stub.")
  (values nil
          :undef))

;; Forwarder endpoint operation
(defmethod engine-endpoint-running-p ((endpoint forwarder-engine-endpoint))
  (terminate-context endpoint)
  (with-slots (processor) endpoint
    (and (endpoint-context endpoint)
         processor
         (threadp processor)
         (thread-alive-p processor))))

(defmethod engine-endpoint-start ((endpoint forwarder-engine-endpoint))
  (log-for (trace) "Starting endpoint: ~A" endpoint)
  (unless (engine-endpoint-running-p endpoint)
    (init-context endpoint)
    (init-sockets endpoint)
    (setf (endpoint-processor endpoint)
          (make-thread #'(lambda ()
                           (loop while :forever do
                                (log-for (warn) "This is the default endpoint processor")
                                (sleep 30)))
                       :name (format nil "engine-endpoint-~A"
                                     (fdog-forwarder-name
                                      (forwarder-engine-forwarder (endpoint-engine endpoint))))))
    :started))

(defmethod engine-endpoint-stop ((endpoint forwarder-engine-endpoint))
  (log-for (trace) "Stopping endpoint: ~A" endpoint)
  (when (engine-endpoint-running-p endpoint)
    (terminate-sockets endpoint)
    (terminate-context endpoint)
    (log-for (warn) "This is very much the wrong way to go about this one.")
    (destroy-thread (endpoint-processor endpoint))
    :stopped))

;; Multibridge Construction and init
(defmethod make-multibridge ((engine forwarder-engine) (handler mongrel2-handler))
  (log-for (trace) "Building multibridge for: ~A" handler)
  (make-instance 'multibridge :engine engine :handler handler
                 :path (mongrel2-route-path (mongrel2-target-route handler))))

;; Multibridge Operation
(defmethod multibridge-running-bridges ((instance multibridge))
  (remove-if-not #'request-handler-running-p (multibridge-bridges instance)))

(defmethod multibridge-idle-bridges ((instance multibridge))
  (remove-if #'request-handler-running-p (multibridge-bridges instance)))

(defmethod multibridge-start ((instance multibridge))
  (unless (multibridge-bridges instance)
    (log-for (trace) "No bridges present, but asked to start. Adding bridge.")
    (multibridge-add-bridge instance))
  (mapc #'request-handler-start (multibridge-idle-bridges instance))
  instance)

(defmethod multibridge-stop ((instance multibridge))
  (mapc #'request-handler-stop (multibridge-running-bridges instance))
  instance)

(defmethod multibridge-configure-new-bridge ((instance multibridge) (bridge fdog-handler:request-handler))
  (log-for (trace) "Configuring bridge: ~A" bridge)
  (request-handler-add-string-responder bridge
                                        (lambda (request)
                                          (log-for (trace) "Serving request for: ~A" instance)
                                          (describe request)
                                          (format nil (multibridge-path instance))))
  bridge)

(defmethod multibridge-add-bridge ((instance multibridge))
  (log-for (trace) "Adding bridge to ~A" instance)
  (let ((bridge (configure-bridges-for (multibridge-handler instance))))
    (multibridge-configure-new-bridge instance bridge)
    (push bridge (multibridge-bridges instance))))

(defmethod multibridge-running-p ((instance multibridge))
  (with-slots (bridges) instance
    (and bridges
         (remove-if-not #'request-handler-running-p bridges))))

;; Engine creation
(defmethod make-forwarder-engine ((forwarder fdog-forwarder) &key servers)
  "Construct a forwarder-engine class from an fdog-forwarder model `forwarder'"
  (log-for (trace) "Building forwarder engine from ~A" forwarder)
  (let ((engine (make-instance 'forwarder-engine :forwarder forwarder :servers servers)))
    (setf (forwarder-engine-endpoint engine)
          (make-instance 'forwarder-engine-endpoint :engine engine))
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
    (remove-if-not #'multibridge-running-p (forwarder-engine-bridges engine))))

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
  (engine-endpoint-start (forwarder-engine-endpoint engine))
  (mapcar #'multibridge-start (forwarder-engine-bridges engine)))

(defmethod forwarder-engine-stop ((engine forwarder-engine))
  (log-for (trace) "Shutting down engine: ~A" engine)
  (engine-endpoint-stop (forwarder-engine-endpoint engine))
  (mapcar #'multibridge-stop (forwarder-engine-bridges engine)))
