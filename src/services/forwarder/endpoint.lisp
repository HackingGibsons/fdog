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
