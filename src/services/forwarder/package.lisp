(defpackage #:fdog-forwarder
  (:use #:cl
        :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)

  (:shadowing-import-from :fdog-models
                          :model-pk
                          :mongrel2-server
                            :mongrel2-server-name
                            :mongrel2-server-port
                            :mongrel2-server-addr
                            :mongrel2-server-ssl
                            :mongrel2-server-default-host
                            :mongrel2-server-default-host-name
                          :mongrel2-host
                            :make-mongrel2-host
                            :mongrel2-host-name
                            :mongrel2-host-matching
                            :mongrel2-host-server
                            :mongrel2-host-routes
                          :mongrel2-handler
                            :mongrel2-handler-recv-ident
                            :mongrel2-handler-send-ident
                            :mongrel2-handler-recv-spec
                            :mongrel2-handler-send-spec
                            :find-mongrel2-handler
                            :make-mongrel2-handler
                          :mongrel2-route
                            :make-host-route
                            :mongrel2-route-path
                            :mongrel2-route-target)
  (:shadowing-import-from :fdog-m2sh
                          :servers :make-server
                          :make-handler)
  (:shadowing-import-from :fdog-handler
                          :request-handler-processors
                          :request-handler-sub
                          :request-handler-pub)
  (:shadowing-import-from :fdog-control
                          :api/endpoint
                          :api/endpoint-with-args
                          :404-condition
                          :header-json-type
                          :with-chunked-stream-reply
                          :interface-start :interface-stop
                          :interface-configure-bridges
                          :fdog-interface
                          :interface-bridge-matching
                          :fdog-interface-bridges)

  (:export :init-forwarders))
(in-package :fdog-forwarder)

(defvar *forwarder-server-name* "forwarder")
(defvar *forwarder-server-port* 13374)
(defvar *forwarder-server-ssl-port* 13375)

(defvar *watchdog-route* "/watchdog/")
(defvar *watchdog-endpoints* '((:send . "tcp://127.0.0.1:1045")
                               (:recv . "tcp://127.0.0.1:1042")))

(defvar *forwarder-zmq-port-base* 30000
  "Ports used to create forwarder external endpoints")
(defvar *handler-zmq-port-base* 50000
  "Ports used to create local handler endpoints for mongrel2 configuration.")

(defvar *forwarders* ()
  "List of the loaded forwarders")


;; Forwarder engine, that which makes the definitions dance
(defclass forwarder-engine ()
  ((forwarder :initarg :forwarder
              :accessor forwarder-engine-forwarder)
   (servers :initarg :servers
            :initform (forwarder-servers)
            :accessor forwarder-engine-servers))
  (:documentation "The engine that manages the forwarding of requests for an endpoint"))

;; Engine creation
(defmethod make-forwarder-engine ((forwarder fdog-forwarder) &key servers)
  "Construct a forwarder-engine class from an fdog-forwarder model `forwarder'"
  (log-for (trace) "Building forwarder engine from ~A" forwarder)
  (make-instance 'forwarder-engine :forwarder forwarder :servers servers))

;; Engine state control
(defgeneric forwarder-engine-running-p (engine)
  (:documentation "Boolean representation of engine operation")
  (:method ((engine forwarder-engine))
    (log-for (warn) "TODO: Not implemented")
    nil))

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
  :undef)

(defmethod forwarder-engine-stop ((engine forwarder-engine))
  (log-for (trace) "Shutting down engine: ~A" engine)
  :undef)


;; General forwarders API
(defmethod init-forwarders (&key (start t))
  (let ((forwarder-servers (ensure-forwarder-environment))
        (forwarders (find-forwarder)))

    (log-for (trace) "Stopping all ~A existing forwarders" (length *forwarders*))
    (forwarder-engine-stop :all)

    (log-for (trace) "Configuring all ~A found forwarders" (length forwarders))
    (configure-forwarder :all :all)

    (setf *forwarders* (mapcar #'(lambda (f) (make-forwarder-engine f :servers forwarder-servers))
                               (remove-if-not #'forwarder-valid-p forwarders)))
    (log-for (trace) "Built ~A/~A engines" (length *forwarders*) (length forwarders))

    (when start
      (log-for (trace) "Starting all forwarder engines")
      (forwarder-engine-start :all))))
