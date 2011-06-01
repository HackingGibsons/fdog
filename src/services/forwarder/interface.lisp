(in-package :fdog-forwarder)

(defclass fdog-forwarding-interface (fdog-interface)
  ((upstream :initarg :upstream
             :accessor forwarder-upstream)
   (context :initform nil
            :initarg :context
            :accessor forwarder-context)
   (response-write-sock :initform nil
                        :initarg :response-write-sock
                        :accessor forwarder-response-write-sock)
   (response-sock :initform nil
                :initarg :listen-sock
                :accessor forwarder-listen-sock)
   (request-sock :initform nil
                 :initarg :request-sock
                 :accessor forwarder-request-sock)
   (response-writer :initform nil
                    :accessor forwarding-interface-response-writer))
  (:documentation "An interface for forwarding requests to upstream 0mq endpoints."))

