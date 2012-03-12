(defpackage #:request-forwarder-agent
  (:documentation "A package for the agent that implements request/response
rewriting, routing and forwarding.")
  (:use :cl
        :log5)

  (:use :afdog
        :agent
        :request-processing-agent)

  (:import-from :arnesi
                :rcurry
                :when-bind)
  (:import-from :alexandria
                :appendf)

  (:export :request-forwarder-agent
           :prefixed-key
           :*request-id-header*
           :agent-request-transform
           :push-state-signal
           :forwarder-endpoint
           :push-sock
           :sub-sock
           :sock-of
           :addr-of
           :push-ready
           :push-state
           :name
           :push-unready
           :deliver-request
           :deliver-response
           :delivery-faulure))

(in-package :request-forwarder-agent)

;; TODO: Find a new home for this
(defmacro with-agent-redis ((agent) &body forms)
  "Handle binding the `agent' specific redis connection to
the redis dynamic var during the execution of `forms'"
  (let ((g!reconnect-handler (gensym "reconnect-handler")))
    `(flet ((,g!reconnect-handler (c)
              "Reconnect handler for a redis connection"
              (log-for (warn) "Reconnecting to Redis!!")
              (let ((reconnect (find-restart :reconnect)))
                (if reconnect
                    (progn
                      (log-for (warn) "Reconnect restart found")
                      (invoke-restart reconnect))
                    (progn
                      (log-for (warn) "There is no reconnect restart")
                      (error c))))))
       (let ((redis:*connection* (redis ,agent)))
         (handler-bind ((redis:redis-connection-error #',g!reconnect-handler))
           ,@forms)))))
