(in-package :api-agent)
(defcategory api-agent)

;; TODO make api-agent use the mixin as well
(defclass api-mixin (request-processing-agent)
  ((server
    :initarg :server
    :accessor server
    :documentation "The name of the mongrel2 server that hosts this API")
   (hosts
    :initarg :hosts
    :accessor hosts
    :documentation "A list of hosts the api handler will listen on")
   (port
    :initarg :port
    :accessor port
    :documentation "The port the mongrel2 server runs on.")
   (route
    :initarg :route
    :accessor route
    :documentation "The route the handler will exist on for the given server."))
  (:default-initargs . (:hosts (list "localhost") :route "/"))
  (:documentation "Mixin for API agents. Common methods specialize on this, while specific API method should specialize on that agent."))

;; TODO add a "listen for mongrel2 server" clause
(defmethod heard-message :after ((agent api-mixin) (head agent-head) (from (eql :agent)) (info (eql :info)) &rest event)
  (let* ((info (getf event :info))
         (provides (getf info :provides))
         (servers (getf provides :servers))
         (server (assoc (server agent) servers :test #'string=)))
    ;; If API server does not exist, announce need
    (when (find :servers provides)
      (unless server
        (send-message head :command `(:command :speak
                                               :say (:agent :need
                                                            :need :server
                                                            :server (:name ,(server agent)
                                                                           :port ,(port agent)
                                                                           :hosts ,(hosts agent)))))
        (log-for (trace api-agent) "Server ~S does not exist, adding" (server agent)))
      ;; If API handler does not exist, announce need
      (multiple-value-bind (name handlers) (values (car server) (cdr server))
        (let ((handler (cdr (assoc (handler-name agent) handlers :test #'string=))))
          (when (and name (not handler))
            (send-message head :command `(:command :speak
                                                   :say (:agent :need
                                                                :need  :handler
                                                                :handler (:server ,(server agent)
                                                                                  :hosts ,(hosts agent)
                                                                                  :route ,(route agent)
                                                                                  :name ,(handler-name agent)))))
            (log-for (trace api-agent) "Server: ~S doesn't have handler ~S, adding." name (handler-name agent))))))))

