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
(defvar *valid-key-regex* "^valid"
  "Regular expression to match valid keys for testing")

(defclass accounts-agent (api-mixin leaf-test-agent)
  ((handler
    :initarg :handler))
  (:default-initargs . (:handle *accounts-handler* :server *accounts-server* :port *accounts-port*))
  (:documentation "API agent subclass to handle mock accounts service."))

(defmethod api/endpoint ((m (eql :post)) (p (eql :|/api_clients/validate|)) (agent accounts-agent) organ handler request raw)
  (let* ((spec (decode-json-from-request (m2cl:request-body request)))
         (api-key (cdr (assoc :api--key spec))))
    (if (ppcre:scan *valid-key-regex* api-key)
        (json:encode-json-alist '((:success . t)))
        (error '401-condition))))
