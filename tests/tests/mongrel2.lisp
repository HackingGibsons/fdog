(in-package :afdog-tests)

(def-test (mongrel2-root-is-not-system-source-directory :group mongrel2-agent-tests) :true
  (not (equalp *root*
               (asdf:system-source-directory :afdog-tests))))

(def-test (mongrel2-agent-starts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture)) :true
  (progn
    (with-agent-conversation (m e :timeout 60) mongrel2-uuid
      (do ((msg (parse-message (read-message m))
                (parse-message (read-message m))))
          (msg))) ;; A message means the thing has booted.

    (fdog-models:connect db-path)

    (let* ((server (fdog-models:servers :refresh t :one t))
           (server-pid (and server (fdog-models:mongrel2-server-pid server)))
           (running-p (fdog-models:mongrel2-server-running-p server)))
      (setf pid server-pid)
      running-p)))

(def-test (mongrel2-agent-restarts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture)) :true
  (progn
    (with-agent-conversation (m e :timeout 60) mongrel2-uuid
      (do ((msg (parse-message (read-message m))
                (parse-message (read-message m))))
          (msg))) ;; A message means the thing has booted.

    (fdog-models:connect db-path)

    (handler-bind ((bt:timeout #'(lambda (c)
                                   (declare (ignorable c))
                                   (error "Timing out")))) ;; nst likes generic errors more.

      ;; TODO: Clean up this horrible copy-pasta. Turn into a fixture or helper. Why not both?
      (let* ((server (fdog-models:servers :refresh t :one t))
             (server-pid (and server (fdog-models:mongrel2-server-pid server)))
             (running-p (fdog-models:mongrel2-server-running-p server)))
        (setf pid server-pid)
        running-p)

      (iolib.syscalls:kill pid iolib.syscalls:sigkill)

      (bt:with-timeout (10)
        (do* ((server (fdog-models:servers :refresh t :one t)
                      (fdog-models:servers :refresh t :one t))
              (server-pid (and server (fdog-models:mongrel2-server-pid server))
                          (and server (fdog-models:mongrel2-server-pid server)))
              (running-p (and server (fdog-models:mongrel2-server-running-p server))
                         (and server (fdog-models:mongrel2-server-running-p server))))
             ((and running-p
                   (not (= pid server-pid)))
              (setf pid server-pid)))))))


