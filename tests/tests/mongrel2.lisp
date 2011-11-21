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
    (:seq :true
          (:not :true)
          :true)
  (list
    (with-agent-conversation (m e) mongrel2-uuid
      (do* ((msg (parse-message (read-message m))
                 (parse-message (read-message m)))
            (process nil (getf msg :process)))
           ((or (and (eql (getf msg :saw) :process)
                     (getf process :pid))
                (and (eql (getf msg :made) :process)
                     (getf msg :pid))) t))) ;; We have a server
      (progn
        (fdog-models:connect db-path)

        ;; Kill the running server.
        (let* ((server (fdog-models:servers :name "control" :refresh t :one t))
               (server-pid (and server (fdog-models:mongrel2-server-pid server)))
               (running-p (fdog-models:mongrel2-server-running-p server)))
          (when running-p
            (setf pid server-pid)
            (fdog-models:mongrel2-server-signal/block server :stop)
            (fdog-models:mongrel2-server-running-p server))))

      ;; Wait for the next process to spawn.
      (with-agent-conversation (m e :timeout 30) mongrel2-uuid
        (do* ((msg (parse-message (read-message m))
                   (parse-message (read-message m)))
              (process nil (getf msg :process))
              (saw-p nil (getf process :saw))
              (saw-pid nil (getf process :pid))
              (made-p nil (getf msg :made))
              (made-pid (getf msg :pid))
              (process-pid (or made-pid saw-pid))
              (server (fdog-models:servers :name "control" :refresh t :one t)
                      (fdog-models:servers :name "control" :refresh t :one t))
              (running-p (fdog-models:mongrel2-server-running-p server)
                         (fdog-models:mongrel2-server-running-p server)))
             ((and running-p
                   (not (= pid process-pid)))
              (setf pid process-pid)
              t)
          (format t "Message ~A.~%Process ~A~%Process Pid ~A~%Pid ~A~%Server ~A~%~%" msg process process-pid pid server))))) ;; a pid we saw or made is not the pid we've had before





