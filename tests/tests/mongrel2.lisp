(in-package :afdog-tests)

(def-test (mongrel2-root-is-not-system-source-directory :group mongrel2-agent-tests) :true
  (not (equalp *root*
               (asdf:system-source-directory :afdog-tests))))

(def-test (mongrel2-agent-starts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture)) :true
  (progn
    (with-agent-conversation (m e) mongrel2-uuid
      (do* ((msg (parse-message (read-message m))
                 (parse-message (read-message m)))
            (process nil (getf msg :process)))
           ((or (and (eql (getf msg :saw) :process)
                     (getf process :pid))
                (and (eql (getf msg :made) :process)
                     (getf msg :pid)))))
    (fdog-models:connect db-path)

    (let* ((server (fdog-models:servers :name "control" :refresh t :one t))
           (server-pid (and server (fdog-models:mongrel2-server-pid server)))
           (running-p (fdog-models:mongrel2-server-running-p server)))
      (setf pid server-pid)
      running-p))))

(def-test (mongrel2-agent-restarts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture))
    (:seq (:eql :first-process)
          (:eql :not-running)
          (:eql :made-new-process)
          (:eql :saw-new-process))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process nil (getf msg :process)))
          ;; TODO: One of these can never match, it destructures the :made message stupid
          ((or (and (eql (getf msg :saw) :process)
                    (getf process :pid))
               (and (eql (getf msg :made) :process)
                    (getf msg :pid)))
           :first-process))) ;; We have a server

   (progn
     (fdog-models:connect db-path)
     ;; Kill the running server.
     (let* ((server (fdog-models:servers :name "control" :refresh t :one t))
            (server-pid (and server (fdog-models:mongrel2-server-pid server)))
            (running-p (fdog-models:mongrel2-server-running-p server)))
       (when running-p
         (setf pid server-pid)
         (fdog-models:mongrel2-server-signal/block server :stop)
         (if (fdog-models:mongrel2-server-running-p server)
             :running
             :not-running))))

      ;; A new process has been made
      (with-agent-conversation (m e :timeout 30) mongrel2-uuid
        (fdog-models:connect db-path)
        (do* ((msg (parse-message (read-message m))
                   (parse-message (read-message m)))
              (made (getf msg :made)
                    (or made (getf msg :made)))
              (made-info (getf msg made)
                         (or made-info (getf msg made)))
              (server (fdog-models:servers :name "control" :refresh t :one t)
                      (fdog-models:servers :name "control" :refresh t :one t))
              (server-pid (and server (fdog-models:mongrel2-server-pid server))
                          (and server (fdog-models:mongrel2-server-pid server))))
             ((and made-info server
                   (fdog-models:mongrel2-server-running-p server)
                   (equalp (fdog-models:mongrel2-server-pid server)
                           (getf made-info :pid)))
              :made-new-process)))


      ;; And that process stuck around.
      (with-agent-conversation (m e :timeout 30) mongrel2-uuid
        (do* ((msg (parse-message (read-message m))
                   (parse-message (read-message m)))
              (saw nil (getf msg :saw))
              (process nil (getf msg :process))
              (saw-pid nil (getf process :pid))
              (server (fdog-models:servers :name "control" :refresh t :one t)
                      (fdog-models:servers :name "control" :refresh t :one t))
              (running-p (fdog-models:mongrel2-server-running-p server)
                         (fdog-models:mongrel2-server-running-p server)))
             ((and (eql saw :process)
                   process
                   running-p
                   (not (= pid saw-pid)))
              (setf pid saw-pid)
              :saw-new-process)))))


(def-test (mongrel2-agent-watches-existing-mongrel :group mongrel2-agent-tests :fixtures (db-path-fixture running-mongrel2-fixture mongrel2-agent-fixture))
    (:seq (:eql :server-is-running)
          (:eql :server-is-watched))
  (list
   (if (fdog-models:mongrel2-server-running-p server)
       (and (setf pid (fdog-models:mongrel2-server-pid server))
            :server-is-running)
       :server-is-not-running)

   (when (fdog-models:mongrel2-server-running-p server)
     (with-agent-conversation (m e :timeout 30) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (saw nil (getf msg :saw))
             (process nil (getf msg :process))
             (saw-pid nil (getf process :pid))
             (server (fdog-models:servers :name "control" :refresh t :one t)
                     (fdog-models:servers :name "control" :refresh t :one t))
             (running-p (fdog-models:mongrel2-server-running-p server)
                        (fdog-models:mongrel2-server-running-p server)))
            ((and (eql saw :process)
                  process
                  running-p
                  (= pid saw-pid))
             :server-is-watched))))))
