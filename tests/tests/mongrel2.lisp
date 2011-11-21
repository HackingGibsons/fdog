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
          (:eql :saw-new-process)
          (:eql :made-new-process))
  (list
    (with-agent-conversation (m e) mongrel2-uuid
      (do* ((msg (parse-message (read-message m))
                 (parse-message (read-message m)))
            (process nil (getf msg :process)))
           ((or (and (eql (getf msg :saw) :process)
                     (getf process :pid))
                (and (eql (getf msg :made) :process)
                     (getf msg :pid)))
            (format t "Msg ~A~%" msg)
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
        (do* ((msg (parse-message (read-message m))
                   (parse-message (read-message m)))
              (made nil (getf msg :made))
              (made-pid (getf msg :pid))
              (server (fdog-models:servers :name "control" :refresh t :one t)
                      (fdog-models:servers :name "control" :refresh t :one t))
              (running-p (fdog-models:mongrel2-server-running-p server)
                         (fdog-models:mongrel2-server-running-p server)))
             ((and (eql made :process)
                   running-p
                   (not (= pid made-pid)))
              (format t "Got Made!~%")
              :made-new-process)
          (format t "Waiting for Made But Got: ~A~%" msg)))


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
              (format t "Found our process!~%")
              :saw-new-process)
          (format t "Message ~A.~%Process ~A~%Process Pid ~A~%Pid ~A~%Server ~A~%~%" msg process saw-pid pid server))))) ;; a pid we saw or made is not the pid we've had before





