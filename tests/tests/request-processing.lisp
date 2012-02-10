(in-package :afdog-tests)

(def-test (request-processing-agent-starts :group request-processing-agent-tests)
    (:seq (:eql :starts)
          (:eql :running))
  (list
   (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info)))
          ((and info)
           :starts)))

   (and (running-p request-processing-runner) :running)))

(def-test (request-processing-agent-sees-mongrel2-agent :group request-processing-agent-tests)
    (:seq
     (:eql :rp-agent-has-peers)
     (:eql :m2-agent-has-peers))
  (list
   (with-agent-conversation (m e :timeout 35) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info))
           (peers (getf info :peers) (getf info :peers)))
          (peers
           :rp-agent-has-peers)))

   (wait-for-agent-message (mongrel2-uuid :timeout 35) (msg)
     (let* ((info (getf msg :info))
            (peers (getf info :peers)))
       (when (assoc request-processing-uuid peers :test #'string-equal)
         :m2-agent-has-peers)))))

(def-test (request-processing-agent-announces-connected-count :group request-processing-agent-tests)
    (:predicate numberp)
  (with-agent-conversation (m e) request-processing-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (requesticle (getf info :requesticle)
                       (getf info :requesticle)))
         (requesticle (getf requesticle :peers)))))

(def-test (request-processing-agent-announces-provides :group request-processing-agent-tests)
    (:equalp "api")
  (with-agent-conversation (m e) request-processing-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :request-processing)))))

(def-test (request-processing-agent-connects-to-matching-handler :group request-processing-agent-tests)
    (:seq (:eql :handler-need-filled)
          (:eql :connected-to-one))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "control" :port *control-port* :hosts ("api.example.com")))))
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "control" :hosts ("api.example.com") :route "/" :name "api"))))
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "control" :hosts ("api.example.com") :route "/ping/" :name "ping"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
          ((and filled
                (getf filled :handler))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :handler-need-filled)))

   (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info)
                 (getf msg :info))
           (requesticle (getf info :requesticle)
                        (getf info :requesticle)))
          ((equalp (getf requesticle :peers) 1)
           :connected-to-one)))))

(def-test (request-processing-agent-can-disable-requesticle :group request-processing-agent-tests)
    (:values (:eql :handler-need-filled)
             (:eql :connected-to-some)
             (:eql :requesticle-disabled)
             (:eql :connected-to-none)
             (:eql :requesticle-enabled)
             (:eql :connected-to-some))

  (with-agent-conversation (m e) mongrel2-uuid
    (zmq:send! e (prepare-message
                  `(:agent :need
                           :need  :server
                           :server (:name "control" :port *control-port* :hosts ("api.example.com")))))
    (zmq:send! e (prepare-message
                  `(:agent :need
                           :need  :handler
                           :handler (:server "control" :hosts ("api.example.com") :route "/" :name "api"))))
    (zmq:send! e (prepare-message
                  `(:agent :need
                           :need  :handler
                           :handler (:server "control" :hosts ("api.example.com") :route "/ping/" :name "ping"))))
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (filled (and (equalp (car msg) :filled) msg)
                  (and (equalp (car msg) :filled) msg)))
         ((and filled
               (getf filled :handler))
          (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
          :handler-need-filled)))

  (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info)
                 (getf msg :info))
           (requesticle (getf info :requesticle)
                        (getf info :requesticle)))
          ((> (getf requesticle :peers) 0)
           :connected-to-some)))

  (and (send-message-blindly request-processing-uuid
                             :request `(:requesticle :disable))
       :requesticle-disabled)

  (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info)
                 (getf msg :info))
           (requesticle (getf info :requesticle)
                        (getf info :requesticle)))
          ((zerop (getf requesticle :peers))
           :connected-to-none)))

  (and (send-message-blindly request-processing-uuid
                             :request `(:requesticle :enable))
       :requesticle-enabled)

  (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info)
                 (getf msg :info))
           (requesticle (getf info :requesticle)
                        (getf info :requesticle)))
          ((> (getf requesticle :peers) 0)
           :connected-to-some))))

(def-test (request-processing-agent-fires-request-handler :group request-processing-agent-tests)
    (:seq (:eql :handler-need-filled)
          (:eql :connected-to-one)
          (:eql :raw-request-handler-fired))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "control" :port *control-port* :hosts ("api.example.com")))))
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "control" :hosts ("api.example.com") :route "/" :name "api"))))
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "control" :hosts ("api.example.com") :route "/ping/" :name "ping"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
          ((and filled
                (getf filled :handler))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :handler-need-filled)))

   (with-agent-conversation (m e) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info)
                 (getf msg :info))
           (requesticle (getf info :requesticle)
                        (getf info :requesticle)))
          ((equalp (getf requesticle :peers) 1)
           :connected-to-one)))

   (with-agent-conversation (m e) request-processing-uuid
     (flet ((ping ()
              (log-for (request-processing-tests trace) "Writing request.")
              (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" *control-port*))
                (write-string (http-request-string "/" :host "api.example.com") (usocket:socket-stream sock))
                (force-output (usocket:socket-stream sock)))
              (log-for (request-processing-tests trace) "Written request.")))
       (do* ((msg (progn (ping) (parse-message (read-message m)))
                  (progn (ping) (parse-message (read-message m)))))
            ((and (getf msg :request-handler)
                  (getf msg :raw)
                  (> (length (getf msg :raw)) 0))
             :raw-request-handler-fired)
         (log-for (request-processing-tests trace) "After-req message: ~S" msg))))))
