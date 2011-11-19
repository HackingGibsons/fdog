(in-package :afdog-tests)

(def-test (mongrel2-agent-starts :group mongrel2-agent-tests :fixtures ()) :true
  (with-agent-conversation (m e :timeout 60) mongrel2-uuid
    (do ((server-pid (fdog-models:mongrel2-server-pid (fdog-models:servers :refresh t :one t))
                     (fdog-models:mongrel2-server-pid (fdog-models:servers :refresh t :one t))))
        (pid pid)
      (when (ignore-errors (iolib.syscalls:kill server-pid 0))
        (setf pid server-pid))
      (sleep 0.1))))
