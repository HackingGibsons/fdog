(defpackage #:fdog-forwarder
  (:use #:cl
        :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)

  (:shadowing-import-from :fdog-handler
                          :request-handler-start
                          :request-handler-stop
                          :request-handler-running-p
                          :request-handler-add-string-responder
                          :configure-bridges-for)
  (:shadowing-import-from :fdog-models
                          :model-pk
                          :mongrel2-server
                            :mongrel2-server-signal
                            :mongrel2-server-signal/block
                            :mongrel2-server-running-p
                            :mongrel2-server-name
                            :mongrel2-server-port
                            :mongrel2-server-addr
                            :mongrel2-server-ssl
                            :mongrel2-server-ssl-p
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
                            :mongrel2-route-target
                            :mongrel2-target-route)
  (:shadowing-import-from :fdog-m2sh
                          :servers :make-server
                          :make-handler)
  (:shadowing-import-from :fdog-handler
                          :request-handler-processors
                          :request-handler-sub
                          :request-handler-pub)
  (:shadowing-import-from :fdog-control
                          :with-dispatch-on
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


