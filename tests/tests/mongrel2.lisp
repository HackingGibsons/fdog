(in-package :afdog-tests)

(def-test (mongrel2-root-is-not-system-source-directory :group mongrel2-agent-tests) :true
  (not (equalp *root*
               (asdf:system-source-directory :afdog-tests))))

(def-test (mongrel2-agent-starts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture)) :true
  (with-agent-conversation (m e :timeout 60) mongrel2-uuid
    (do ()
        ((and (probe-file db-path)
              (fdog-models:connect db-path)
              (find "SERVER" (clsql:list-tables)))
         (setf connected-p t)))

    (if connected-p
        (do ((server-pid (fdog-models:mongrel2-server-pid (fdog-models:servers :refresh t :one t))
                         (fdog-models:mongrel2-server-pid (fdog-models:servers :refresh t :one t))))
            (pid pid)
          (when (ignore-errors (iolib.syscalls:kill server-pid 0))
            (setf pid server-pid)
            pid)
          (sleep 0.1))
        nil)))
