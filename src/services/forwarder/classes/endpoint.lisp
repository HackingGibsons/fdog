(in-package :fdog-forwarder)

;; Forwarder endpoint, a proxy between the handlers and an outside consumer
(defvar *endpoint-socket-linger* 1000)
(defclass forwarder-engine-endpoint ()
  ((engine :initarg :engine
           :accessor endpoint-engine)

   (request-device :initform nil
                   :accessor endpoint-request-device)
   (response-device :initform nil
                    :accessor endpoint-response-device)

   ;; ZMQ Context for this endpoint
   (context-threads :initargs :threads :initform 4
                    :accessor endpoint-context-threads)
   (context :initarg :context
            :initform nil
            :accessor endpoint-context)

   ;; Proxies
   ;; Request forwarding
   (request-proxy-addr :initarg :proxy-addr
                       :accessor endpoint-proxy-addr)
   (request-proxy-sock :initarg :proxy-sock :initform nil
                       :accessor endpoint-proxy-sock)
   ;; Response proxy
   (response-proxy-sock :initarg :response-proxy-sock :initform nil
                        :accessor endpoint-response-proxy-sock)

   ;; Public facing sockets
   (request-sock :initarg :push-sock :initform nil
                 :accessor endpoint-request-sock)
   (response-sock :initarg :sub-sock :initform nil
                  :accessor endpoint-response-sock)))


;; TODO: Put these redis constants elsewhere.
(defclass forwarder-queue-endpoint (forwarder-engine-endpoint)
  ((redis-host :initform "localhost"
               :accessor queue-endpoint-redis-host)
   (redis-port :initform 6379
               :accessor queue-endpoint-redis-port)
   (request-prefix :initform "fdog-request:")
   (queue-prefix :initform "fdog-queue:")

   ;; Extra thread for drawing requests from the queue and writing to the client
   (request-write-device :initform nil
                         :accessor endpoint-request-write-device))

  (:documentation "Endpoint that instead of writing requests upstream, writes
them to redis and forwards from a different thread."))
