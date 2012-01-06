(in-package :afdog-tests)

(def-test (mongrel2-root-is-not-system-source-directory :group mongrel2-agent-tests) :true
  (not (equalp *root*
               (asdf:system-source-directory :afdog-tests))))

(def-test (mongrel2-agent-starts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :starts)
          (:eql :running))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process)))
          ((or (and (equalp (getf msg :saw) :process)
                    (getf process :pid))
               (and (equalp (getf msg :made) :process)
                    (getf process :pid)))
           (setf pid (getf process :pid))
           :starts)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "control" :refresh t :one t)
                   (fdog-models:servers :name "control" :refresh t :one t)))
          ((and server (getf process :pid)
                (equalp (getf process :pid)
                        (fdog-models:mongrel2-server-pid server)))
           :running)))))

(def-test (mongrel2-agent-restarts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :first-process)
          (:eql :not-running)
          (:eql :made-new-process)
          (:eql :saw-new-process))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process)))
          ;; TODO: One of these can never match, it destructures the :made message stupid
          ((or (and (eql (getf msg :saw) :process)
                    (getf process :pid))
               (and (eql (getf msg :made) :process)
                    (getf process :pid)))
           :first-process))) ;; We have a server

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "control" :refresh t :one t)
                   (fdog-models:servers :name "control" :refresh t :one t)))
          ((and server (getf process :pid)
                (equalp (getf process :pid)
                        (fdog-models:mongrel2-server-pid server)))

           (setf pid (getf process :pid))
           (handler-case
               (fdog-models:mongrel2-server-signal/block server :stop)
             (t (c)
               (format t "Error stopping mongrel2: ~A~%" c)))
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


(def-test (mongrel2-agent-watches-and-restarts-existing-mongrel :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :first-process)
          (:eql :runner-dead)
          (:eql :same-server-found)
          (:eql :mongrel2-dead)
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
           (setf pid (or (getf process :pid) (getf msg :pid)))
           :first-process))) ;; We have a server

   (progn
     (stop mongrel2-runner)
     (unless (running-p mongrel2-runner)
       :runner-dead))

   (progn
     (fdog-models:connect db-path)
     (setf mongrel2-runner (make-runner :test :include '(:afdog-tests)
                                        :class 'mongrel2-test-agent
                                        :root *root* ;; different root for the test agents
                                        :uuid mongrel2-uuid))

     (start mongrel2-runner)

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
             :same-server-found))))

   (progn
     (let ((server (fdog-models:servers :name "control" :refresh t :one t)))
       (fdog-models:mongrel2-server-signal/block server :stop)
       (if (fdog-models:mongrel2-server-running-p server)
           :mongrel2-running
           :mongrel2-dead)))

   ;; A new process has been made
   (with-agent-conversation (m e :timeout 30) mongrel2-uuid
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "control" :refresh t :one t)
                   (fdog-models:servers :name "control" :refresh t :one t))
           (server-pid (and server (fdog-models:mongrel2-server-pid server))
                       (and server (fdog-models:mongrel2-server-pid server))))
          ((and (getf process :pid) server
                (fdog-models:mongrel2-server-running-p server)
                (equalp (fdog-models:mongrel2-server-pid server)
                        (getf process :pid)))
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

(def-test (mongrel2-agent-announces-mongrels :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :find-pid)
          (:eql :announce-servers))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process nil (getf msg :process)))
          ;; TODO: One of these can never match, it destructures the :made message stupid
          ((or (and (eql (getf msg :saw) :process)
                    (getf process :pid))
               (and (eql (getf msg :made) :process)
                    (getf process :pid)))
           (setf pid (getf process :pid))
           :find-pid)))

   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (infop (equalp (car msg) :agent) (equalp (car msg) :agent))
           (info (getf msg :info) (getf msg :info))
           (provides (and infop (getf info :provides))
                     (and infop (getf info :provides))))

          ((and infop info provides
                (getf provides :servers))
           :announce-servers)))))

(def-test (mongrel2-agent-solves-need-of-existing-server :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :find-pid)
          (:eql :need-filled)
          (:eql :find-new-host))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process nil (getf msg :process)))
          ((or (and (eql (getf msg :saw) :process)
                    (getf process :pid))
               (and (eql (getf msg :made) :process)
                    (getf process :pid)))
           (setf pid (getf process :pid))
           :find-pid)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "control" :port 6767 :hosts ("api.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "control" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))))

(def-test (mongrel2-agent-creates-new-server :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :find-new-host))
  (list
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
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))))

(def-test (mongrel2-agent-new-server-starts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :find-new-host)
          (:eql :server-running))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 7171 :hosts ("awesome.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "awesome.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t)))
          ((and server (getf process :pid)
                (equalp (getf process :pid)
                        (fdog-models:mongrel2-server-pid server)))
           :server-running)))))


(def-test (mongrel2-agent-can-remove-server :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :server-running)
          (:eql :remove-need-filled)
          (:eql :server-removed))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 7171 :hosts ("awesome.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t)))
          ((and server (getf process :pid)
                (equalp (getf process :pid)
                        (fdog-models:mongrel2-server-pid server)))
           :server-running)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :remove-server
                            :remove-server ((:name "forwarder")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :remove-server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :remove-need-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1))))
          ((not (fdog-models:servers :name "forwarder" :refresh t :one t))
           :server-removed)))))

(def-test (mongrel2-agent-can-remove-host :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :find-pid)
          (:eql :need-filled)
          (:eql :find-new-hosts)
          (:eql :remove-need-filled)
          (:eql :host-removed))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process nil (getf msg :process)))
          ((or (and (eql (getf msg :saw) :process)
                    (getf process :pid))
               (and (eql (getf msg :made) :process)
                    (getf process :pid)))
           (setf pid (getf process :pid))
           :find-pid)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "control" :port 6767 :hosts ("api.example.com"
                                                                        "api2.example.com"
                                                                        "api3.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "control" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            (find "api3.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-hosts)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :remove-host
                            :remove-host (:server "control" :host "api2.example.com"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :remove-host))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :remove-need-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1)))
           (info (getf msg :info) (getf msg :info)))
          ((let* ((server (fdog-models:servers :name "control" :refresh t :one t))
                  (hosts (and server (fdog-models:mongrel2-server-hosts server))))
             (not (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)))
           :host-removed)))))

(def-test (mongrel2-agent-can-remove-last-host :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :find-new-host)
          (:eql :server-running)

          (:eql :removed-host)
          (:eql :server-gone))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 7171 :hosts ("awesome.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "awesome.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t)))
          ((and server (getf process :pid)
                (equalp (getf process :pid)
                        (fdog-models:mongrel2-server-pid server)))
           :server-running)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :remove-host
                            :remove-host (:server "forwarder" :host "awesome.example.com"))))

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :remove-host))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :removed-host)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1))))
           ((not (fdog-models:servers :name "forwarder" :refresh t :one t))
            :server-gone)))))

(def-test (mongrel2-agent-culls-servers :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :find-new-host)
          (:eql :removal-filled)
          (:eql :server-gone))
  (list
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
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :keep-servers
                            :keep-servers ("control"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :keep-servers))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :removal-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1))))
           ((not (fdog-models:servers :name "forwarder" :refresh t :one t))
            :server-gone)))))

(def-test (mongrel2-agent-culls-all-servers :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :find-new-host)
          (:eql :removal-filled)
          (:eql :servers-gone))
  (list
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
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :keep-servers
                            :keep-servers ())))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :keep-servers))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :removal-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1))))
           ((not (fdog-models:servers :refresh t))
            :servers-gone)))))

(def-test (mongrel2-agent-culls-hosts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :need-filled)
          (:eql :find-new-host)
          (:eql :removal-filled)
          (:eql :hosts-gone))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 6969 :hosts ("api.example.com"
                                                                          "api2.example.com"
                                                                          "api3.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :server))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :need-filled)))

   (progn
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
            (hosts (and server (fdog-models:mongrel2-server-hosts server))))
       (and (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            (find "api3.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
            :find-new-host)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :keep-hosts
                            :keep-hosts (:server "forwarder" :hosts ("api.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :keep-hosts))
           (log-for (trace mongrel2-agent::agent-needs) "Filled hosts: ~A" msg)
           :removal-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1)))
           (info (getf msg :info) (getf msg :info)))
          ((let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
                  (hosts (and server (fdog-models:mongrel2-server-hosts server))))
             (and (not (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name))
                  (not (find "api3.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name))
                  (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)))
           :hosts-gone)))))

(def-test (mongrel2-agent-can-make-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :server-need-filled)
          (:eql :server-found)
          (:eql :handler-need-filled)
          (:eql :host-found)
          (:eql :route-found)
          (:eql :handler-found)
          (:eql :handler-has-endpoint))
  (list
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
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :server-need-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (process (getf msg :process)
                    (getf msg :process))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t)))
          ((and server (getf process :pid)
                (equalp (getf process :pid)
                        (fdog-models:mongrel2-server-pid server)))
           :server-found)))

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

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1)))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t)))
          ((and server
                (fdog-models:find-mongrel2-host server "api.example.com"))
           :host-found)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1)))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t))
           (host (when server (fdog-models:find-mongrel2-host server "api.example.com"))
                 (when server (fdog-models:find-mongrel2-host server "api.example.com"))))
          ((and host
                (fdog-models:find-mongrel2-route host "/"))
           :route-found)))

   (with-agent-conversation (m e) mongrel2-uuid
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)

     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1)))
           (server (fdog-models:servers :name "forwarder" :refresh t :one t)
                   (fdog-models:servers :name "forwarder" :refresh t :one t)))
          ((and server
                (fdog-models:find-mongrel2-handler :ident "api" :exact nil))
           :handler-found)))

   (let* ((server (fdog-models:servers :one t :refresh t :name "forwarder"))
          (host (and server (fdog-models:find-mongrel2-host server "api.example.com")))
          (route (and host (fdog-models:find-mongrel2-route host "/")))
          (target (and route (fdog-models:mongrel2-route-target route))))
     (and target
          (fdog-models:mongrel2-handler-send-spec target)
          (fdog-models:mongrel2-handler-recv-spec target)
          (prog1 :handler-has-endpoint
            (log-for (trace mongrel2-agent::agent-needs) "Target: ~A" target))))))

(def-test (mongrel2-agent-announces-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :server-need-filled)
          (:eql :handler-need-filled)
          (:eql :handler-announced))
  (list
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
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :server-need-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "forwarder" :hosts ("api.example.com") :route "/admin/" :name "api-admin"))))
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

   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m :timeout 1))
                (parse-message (read-message m :timeout 1)))
           (infop (equalp (car msg) :agent) (equalp (car msg) :agent))
           (info (getf msg :info) (getf msg :info))
           (provides (and infop (getf info :provides))
                     (and infop (getf info :provides)))
           (servers (getf provides :servers)
                    (getf provides :servers)))
          ((and provides servers)
           (let* ((server (assoc "forwarder" servers :test #'string=))
                  (server-info (cdr server))

                  (api (assoc "api" server-info :test #'string=))
                  (api-info (cdr api)))
             (and api-info
                  (getf api-info :send)
                  (getf api-info :recv)
                  :handler-announced)))))))

(def-test (mongrel2-agent-updates-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :handler-filled)
          (:eql :handler-refilled)
          (:eql :handler-updated))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
          ((and filled
                (getf filled :handler))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :handler-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "forwarder" :hosts ("api.example.com") :route "/magic/" :name "api"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :handler))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :handler-refilled)))

   (progn
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :one t :refresh t :name "forwarder"))
            (host (and server (fdog-models:find-mongrel2-host server "api.example.com")))
            (old-route (and host (fdog-models:find-mongrel2-route host "/")))
            (route (and host (fdog-models:find-mongrel2-route host "/magic/")))
            (target (and route (fdog-models:mongrel2-route-target route))))
       (cond (old-route
              :old-route-exists)

             ((and route target)
              :handler-updated))))))

(def-test (mongrel2-agent-removes-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
    (:seq (:eql :handler-filled)
          (:eql :handler-removed)
          (:eql :handler-missing))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :handler
                            :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
          ((and filled
                (getf filled :handler))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :handler-filled)))

   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :remove-handler
                            :remove-handler (:server "forwarder" :name "api"))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :remove-handler))
           (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
           :handler-removed)))

   (progn
     (ignore-errors (fdog-models:disconnect))
     (ignore-errors (clsql:disconnect))
     (fdog-models:connect db-path)
     (let* ((server (fdog-models:servers :one t :refresh t :name "forwarder"))
            (host (and server (fdog-models:find-mongrel2-host server "api.example.com")))
            (old-route (and host (fdog-models:find-mongrel2-route host "/")))
            (old-handler (fdog-models:find-mongrel2-handler :ident "api" :exact nil)))
       (and (not old-route)
            (not old-handler)
            :handler-missing)))))
