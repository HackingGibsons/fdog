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

(def-test (api-agent-builds-api-handler :group api-agent-tests)
    (:seq
     (:equalp "api")
     (:all (:apply car (:equalp :send))
           (:apply cdr (:predicate stringp)))
     (:all (:apply car (:equalp :recv))
           (:apply cdr (:predicate stringp))))
  (wait-for-agent-message (mongrel2-uuid) (msg)
    (let* ((control (cdr (assoc "control"
                                (getf (getf (getf msg :info) :provides) :servers)
                                :test #'string=)))
           (api-info (assoc "api" control :test #'string=))
           (api-name (car api-info))
           (send-info (assoc :send (cdr api-info)))
           (recv-info (assoc :recv (cdr api-info))))
      (list  api-name
             send-info
             recv-info))))

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
              (usocket:with-connected-socket (sock (usocket:socket-connect "localhost" *control-port*))
                (write-string (http-request-string "/" :host "api.example.com") (usocket:socket-stream sock))
                (force-output (usocket:socket-stream sock)))))

       (do* ((msg (progn (ping) (parse-message (read-message m)))
                  (progn (ping) (parse-message (read-message m)))))
            ((and (getf msg :request-handler)
                  (getf msg :raw)
                  (> (length (getf msg :raw)) 0))
             :raw-request-handler-fired)))))

(def-test (api-agent-sends-404 :group api-agent-tests)
    (:values (:eql :connected-to-one)
             (:equalp 404))

  (wait-for-agent-message (api-uuid) (msg)
    (aand (getf (getf (getf msg :info) :requesticle) :peers)
          (and (numberp it) (>= it 1))
          :connected-to-one))

  (bt:with-timeout (5)
    (handler-case
        (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
            (drakma:http-request (format nil "http://localhost:~A/not-found/" *control-port*) :want-stream t)
          (declare (ignorable headers uri stream reason-phrase))
          (when must-close
            (close body-or-stream))
          status-code)
      (bt:timeout () :timeout))))

(def-test (api-responds-with-version :group api-agent-tests)
    (:values (:eql :connected-to-one)
             (:seq (:equalp 200)
                   (:predicate stringp)))

  (wait-for-agent-message (api-uuid) (msg)
    (aand (getf (getf (getf msg :info) :requesticle) :peers)
          (and (numberp it) (>= it 1))
          :connected-to-one))

  (bt:with-timeout (5)
    (handler-case
        (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
            (drakma:http-request (format nil "http://localhost:~A/" *control-port*))
          (declare (ignorable headers uri stream must-close reason-phrase))
          (let ((data (and body (json:decode-json-from-string (babel:octets-to-string body)))))
            (list status-code (cdr (assoc :version data)))))
      (bt:timeout () :timeout))))
