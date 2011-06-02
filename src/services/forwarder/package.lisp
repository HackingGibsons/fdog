(defpackage #:fdog-forwarder
  (:use #:cl
        :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)

  (:shadowing-import-from :fdog-models
                          :model-pk
                          :mongrel2-server
                            :mongrel2-server-default-host
                            :mongrel2-server-default-host-name
                          :mongrel2-host
                            :mongrel2-host-server
                            :mongrel2-host-routes
                          :mongrel2-route
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

(defvar *forwarders* ()
  "List of the loaded forwarders")

