(defpackage #:afdog-accounts
  (:use :cl)
  (:documentation "Mock accounts service.")
  (:use :afdog
        :agent
        :log5
        :request-processing-agent
        :http-dog))

(in-package :afdog-accounts)

;;;; Mock accounts service
;; Expectations:
;; Valid api keys - "validkey", "goodkey"
;; Invalid api key - anything else (use "badkey" or "invalidkey" in
;; testing to make the intention clear
;;
;; If any request is sent with an invalid api key, reject with a 401
;; error
;;
;; Respond to the path /api_clients/validate
;;
;; Valid header api key:
;; - invalid service - 404
;; - valid service, invalid api key - 400

(defvar *accounts-handler* "accounts")
(defvar *accounts-server* "accounts")
(defvar *accounts-port* 1338)
(defvar *accounts-host* "localhost")

;; TODO make api-agent use the mixin as well
;; TODO move me
(defclass api-mixin (request-processing-agent)
  ((handler
    :initarg :handler
    :documentation "The name of the mongrel2 handler")
   (server
    :initarg :server
    :documentation "The name of the mongrel2 server that hosts this API")
   (hosts
    :initarg :hosts
    :documentation "A list of hosts the api handler will listen on")
   (port
    :initarg :port
    :documentation "The port the mongrel2 server runs on.")
   (route
    :initarg :route
    :documentation "The route the handler will exist on for the given server."))
  (:default-initargs . (:hosts (list "localhost") :route "/"))
  (:documentation "Mixin for API agents. Common methods specialize on this, while specific API method should specialize on that agent."))

(defclass accounts-agent (api-mixin leaf-test-agent)
  ((handler
    :initarg :handler))
  (:default-initargs . (:handle *accounts-handler*))
  (:documentation "API agent subclass to handle mock accounts service."))

(defmethod api/endpoint ((m (eql :post)) (p (eql :|/api_clients/validate|)) (agent accounts-agent) organ handler request raw)
  )
