(in-package :afdog-tests)

(def-test (request-processing-agent-starts :group request-processing-agent-tests :fixtures (request-processing-agent-fixture kill-everything-fixture))
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

(def-test (request-processing-agent-sees-mongrel2-agent :group request-processing-agent-tests :fixtures (db-path-fixture  mongrel2-agent-fixture request-processing-agent-fixture kill-everything-fixture))
    (:seq
     (:eql :informed-peers)
     (:eql :rp-agent-has-peers)
     (:eql :m2-agent-has-peers))
  (list
   (and (tell-agent-about request-processing-uuid mongrel2-uuid)
        :informed-peers)

   (with-agent-conversation (m e :timeout 35) request-processing-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info))
           (peers (getf info :peers) (getf info :peers)))
          (peers
           :rp-agent-has-peers)))

   (with-agent-conversation (m e :timeout 35) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info))
           (peers (getf info :peers) (getf info :peers)))
          (peers
           :m2-agent-has-peers)))))

(def-test (request-processing-agent-announces-connected-count :group request-processing-agent-tests :fixtures (request-processing-agent-fixture kill-everything-fixture))
    (:eql 0)
  (with-agent-conversation (m e) request-processing-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (requesticle (getf info :requesticle)
                       (getf info :requesticle)))
         (requesticle (getf requesticle :peers)))))

(def-test (request-processing-agent-announces-provides :group request-processing-agent-tests :fixtures (request-processing-agent-fixture kill-everything-fixture))
    (:equalp "api")
  (with-agent-conversation (m e) request-processing-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :request-processing)))))

(def-test (request-processing-agent-connects-to-matching-handler
           :group request-processing-agent-tests
           :fixtures (db-path-fixture mongrel2-agent-fixture request-processing-agent-fixture kill-everything-fixture))
    (:seq (:eql :informed-peers)
          (:eql :server-need-filled)
          (:eql :handler-need-filled)
          (:eql :connected-to-one))
  (list
   (and (tell-agent-about request-processing-uuid mongrel2-uuid)
        :informed-peers)

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           :server-need-filled)))


   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
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
