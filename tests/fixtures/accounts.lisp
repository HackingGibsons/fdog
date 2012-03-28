(in-package :afdog-tests)

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

;; valid API keys
(defclass accounts-agent (api-mixin leaf-test-agent)
  ()
  (:default-initargs . (:handle *accounts-handler* :server *accounts-server* :port *accounts-port*))
  (:documentation "API agent subclass to handle mock accounts service."))

(in-package :api-app)

(defvar *api-key-header* "X-API-Key")
(defvar *valid-key-regex* "^valid"
  "Regular expression to match valid keys for testing")

(defmethod api/endpoint-with-args ((m (eql :post)) (p (eql :|/keys|)) rest (agent afdog-tests:accounts-agent) organ handler request raw)
  "Destructure the key name from the path to pass to /validate/."
  (ppcre:register-groups-bind (api-key rest) ("^/?([^/]+)(/?.*$)" rest)
    (with-dispatch-on rest &route
        (funcall &route agent organ handler request api-key rest)
      (:exact "/validate/" :responder 'api/keys/validate)
      (:404 :responder 'api/keys/404))))

(defmethod api/keys/404 ((agent api-agent) organ handler request api-key rest)
  "API key 404"
  (error '404-condition
         :details (format nil "No resource for API key ~A matching ~A" api-key rest)))

(defmethod api/keys/validate ((agent afdog-tests:accounts-agent) organ handler request api-key rest)
  "Validate API key by the given regex."
  (if (and (ppcre:scan *valid-key-regex* (m2cl:request-header request (string-downcase *api-key-header*))) (ppcre:scan *valid-key-regex* api-key))
      (with-chunked-stream-reply (handler request stream
                                          :headers ((header-json-type)))
        (json:encode-json-alist '((:success . t)) stream))
      (error '401-condition)))
