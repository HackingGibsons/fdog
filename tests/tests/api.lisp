(in-package :afdog-tests)

(def-test (api-agent-starts :group api-agent-tests)
    (:seq (:eql :starts)
          (:eql :running))
  (list
   (with-agent-conversation (m e) api-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (info (getf msg :info) (getf msg :info)))
          ((and info)
           :starts)))

   (and (running-p api-runner) :running)))

(def-test (api-agent-announces-provides :group api-agent-tests)
    (:equalp "api")
  (with-agent-conversation (m e) api-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (provides (getf info :provides)
                    (getf info :provides)))
         (provides (getf provides :request-processing)))))

(def-test (api-agent-builds-api-forwarder :group api-agent-tests)
    (:all (:apply car (:equalp "api"))
          (:apply cdr (:permute (:seq (:equalp :send)
                                      (:predicate stringp)
                                      (:equalp :recv)
                                      (:predicate stringp)))))
  (wait-for-agent-message (mongrel2-uuid) (msg)
    (let* ((control (cdr (assoc "control"
                               (getf (getf (getf msg :info) :provides) :servers)
                               :test #'string=))))
      (assoc "api" control :test #'string=))))

(def-test (api-agent-fires-request-handler :group api-agent-tests)
    (:values (:eql :connected-to-one)
             (:eql :raw-request-handler-fired))

  (with-agent-conversation (m e) api-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (info (getf msg :info)
                (getf msg :info))
          (requesticle (getf info :requesticle)
                       (getf info :requesticle)))
         ((>= (getf requesticle :peers) 1)
          :connected-to-one)))

  (with-agent-conversation (m e) api-uuid
     (flet ((ping ()
              (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" 6767))
                (write-string (http-request-string "/" :host "api.example.com") (usocket:socket-stream sock))
                (force-output (usocket:socket-stream sock)))))

       (do* ((msg (progn (ping) (parse-message (read-message m)))
                  (progn (ping) (parse-message (read-message m)))))
            ((and (getf msg :request-handler)
                  (getf msg :raw)
                  (> (length (getf msg :raw)) 0))
             :raw-request-handler-fired)))))
