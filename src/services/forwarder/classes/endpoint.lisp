(in-package :fdog-forwarder)

;; Forwarder endpoint, a proxy between the handlers and an outside consumer
(defvar *endpoint-socket-linger* 1000)
(defclass forwarder-engine-endpoint ()
  ((engine :initarg :engine
           :accessor endpoint-engine)
   (alias :initarg :alias
          :accessor endpoint-alias
          :initform nil)

   (request-device :initform nil
                   :accessor endpoint-request-device)
   (response-device :initform nil
                    :accessor endpoint-response-device)

   ;; ZMQ Context for this endpoint
   (context-threads :initargs :threads :initform 1
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
   (response-proxy-addr :initarg :response-proxy-addr :initform nil
                        :accessor endpoint-response-proxy-addr)
   (response-proxy-sock :initarg :response-proxy-sock :initform nil
                        :accessor endpoint-response-proxy-sock)

   ;; Public facing sockets
   (request-sock :initarg :push-sock :initform nil
                 :accessor endpoint-request-sock)
   (response-sock :initarg :sub-sock :initform nil
                  :accessor endpoint-response-sock)))


;; TODO: Put these redis constants elsewhere.
(defclass forwarder-queue-endpoint (forwarder-engine-endpoint)
  ((request-linger :initform 300
                   :accessor queue-endpoint-request-linger)
   (response-linger :initform 300
                   :accessor queue-endpoint-response-linger)
   (queue-prefix :initform "fdog-queue:")

   ;; New sink for requests, handlers should write here to get written to redis.
   (request-queue-sock :initarg :queue-sock ;; pull sock, from here to redis.
                       :initform nil
                       :accessor endpoint-queue-sock)
   (request-queue-addr :initarg :queue-addr
                       :initform nil
                       :accessor endpoint-queue-addr)

   ;; Extra thread for drawing requests from the queue and writing to the client
   ;; this device writes requests from redis to the `request-queue-sock'
   (request-write-device :initform nil
                         :accessor endpoint-request-write-device)
   (request-queue-device :initform nil
                         :accessor endpoint-request-queue-device)

   (response-logging-device :initform nil
                            :accessor endpoint-response-logging-device))

  (:documentation "Endpoint that instead of writing requests upstream, writes
them to redis and forwards from a different thread."))
