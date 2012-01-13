(in-package :afdog-tests)

(def-test (mongrel2-root-is-not-system-source-directory :group mongrel2-agent-tests)
    (:not (:equalp (asdf:system-source-directory :afdog-tests)))
  *root*)

(def-test (mongrel2-agent-starts :group mongrel2-agent-tests)
    (:eql :running)
  (with-agent-conversation (m e) mongrel2-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (process (getf msg :process)
                   (getf msg :process)))
         ((or (and (equalp (getf msg :saw) :process)
                   (getf process :pid))
              (and (equalp (getf msg :made) :process)
                   (getf process :pid)))
          :running))))

(def-test (mongrel2-agent-restarts-mongrel :group mongrel2-agent-tests)
    (:seq (:eql :killed)
          (:eql :started)
          (:eql :running))
  (let (pid)
    (list
     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            (m2pid
             (setf pid m2pid)
             (ignore-errors (iolib.syscalls:kill m2pid iolib.syscalls:sigkill))
             :killed)))

     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            ((and m2pid (not (equalp m2pid pid)))
             (setf pid m2pid)
             :started)))

     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            ((and m2pid (equalp m2pid pid))
             :running))))))

(def-test (mongrel2-agent-watches-and-restarts-existsting-mongrel :group mongrel2-agent-tests)
    (:seq (:eql :have-process)
          (:eql :killed-agent)
          (:eql :agent-returns)
          (:eql :watching-process)
          (:eql :killed)
          (:eql :started)
          (:eql :running))
  (let (pid agent-age)
    (list
     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            (m2pid
             (setf pid m2pid)
             :have-process)))

     (and (with-agent-conversation (m e :linger -1) mongrel2-uuid
            (do* ((msg (parse-message (read-message m))
                       (parse-message (read-message m)))
                  (info (getf msg :info)
                        (getf msg :info))
                  (age (getf info :age)
                       (getf info :age)))
                 (age
                  (setf agent-age age)
                  (zmq:send! e (prepare-message `(:agent :kill :kill ,mongrel2-uuid))))))
          (with-agent-conversation (m e) hypervisor-uuid
            (do* ((msg (parse-message (read-message m))
                       (parse-message (read-message m)))
                  (info (getf msg :info)
                        (getf msg :info)))
                 ((not (getf info :peers))
                  :killed-agent))))

     (with-agent-conversation (m e :timeout 60) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (info (getf msg :info)
                   (getf msg :info))
             (age (getf info :age)
                  (getf info :age)))
            (age
             (if (< age agent-age)
                 :agent-returns
                 :agent-too-old))))

     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            (m2pid
             (and (equalp pid m2pid)
             :watching-process))))

     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            (m2pid
             (ignore-errors (iolib.syscalls:kill m2pid iolib.syscalls:sigkill))
             :killed)))

     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            ((and m2pid (not (equalp m2pid pid)))
             (setf pid m2pid)
             :started)))

     (with-agent-conversation (m e) mongrel2-uuid
       (do* ((msg (parse-message (read-message m))
                  (parse-message (read-message m)))
             (process (getf msg :process)
                      (getf msg :process))
             (m2pid (getf (getf msg :process) :pid)
                    (getf (getf msg :process) :pid)))
            ((and m2pid (equalp m2pid pid))
             :running))))))

(def-test (mongrel2-agent-announces-mongrels :group mongrel2-agent-tests)
    (:eql :announce-servers)
  (with-agent-conversation (m e) mongrel2-uuid
    (do* ((msg (parse-message (read-message m))
               (parse-message (read-message m)))
          (infop (equalp (car msg) :agent) (equalp (car msg) :agent))
          (info (getf msg :info) (getf msg :info))
          (provides (and infop (getf info :provides))
                    (and infop (getf info :provides))))

         ((and infop info provides
               (getf provides :servers))
          :announce-servers))))

(def-test (mongrel2-agent-solves-need-of-existing-server :group mongrel2-agent-tests)
    (:seq (:eql :need-filled)
          (:eql :find-new-host))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "control" :port 6767 :hosts ("api.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
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

(def-test (mongrel2-agent-creates-new-server :group mongrel2-agent-tests)
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
                   (and (equalp (car msg) :filled) msg)))
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

(def-test (mongrel2-agent-new-server-starts :group mongrel2-agent-tests)
    (:seq (:eql :need-filled)
          (:eql :find-new-host)
          (:eql :server-running))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 6969 :hosts ("awesome.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
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

;; HERE BE DRAGONS
;; TODO: The bellow tests are being ported to a supervised and persistent
;;       mongrel2-agent They are being deleted as they are ported.
(def-test (mongrel2-agent-can-remove-server :group mongrel2-agent-tests)
    (:seq (:eql :need-filled)
          (:eql :server-running)
          (:eql :remove-need-filled)
          (:eql :server-removed))
  (list
   (with-agent-conversation (m e) mongrel2-uuid
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need  :server
                            :server (:name "forwarder" :port 6969 :hosts ("awesome.example.com")))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
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
                   (and (equalp (car msg) :filled) msg)))
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

;; (def-test (mongrel2-agent-can-remove-host :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :find-pid)
;;           (:eql :need-filled)
;;           (:eql :find-new-hosts)
;;           (:eql :remove-need-filled)
;;           (:eql :host-removed))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (process nil (getf msg :process)))
;;           ((or (and (eql (getf msg :saw) :process)
;;                     (getf process :pid))
;;                (and (eql (getf msg :made) :process)
;;                     (getf process :pid)))
;;            (setf pid (getf process :pid))
;;            :find-pid)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "control" :port 6767 :hosts ("api.example.com"
;;                                                                         "api2.example.com"
;;                                                                         "api3.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :need-filled)))

;;    (progn
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :name "control" :refresh t :one t))
;;             (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;        (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             (find "api3.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             :find-new-hosts)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :remove-host
;;                             :remove-host (:server "control" :host "api2.example.com"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :remove-host))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :remove-need-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1)))
;;            (info (getf msg :info) (getf msg :info)))
;;           ((let* ((server (fdog-models:servers :name "control" :refresh t :one t))
;;                   (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;              (not (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)))
;;            :host-removed)))))

;; (def-test (mongrel2-agent-can-remove-last-host :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :need-filled)
;;           (:eql :find-new-host)
;;           (:eql :server-running)

;;           (:eql :removed-host)
;;           (:eql :server-gone))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 7171 :hosts ("awesome.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :need-filled)))

;;    (progn
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
;;             (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;        (and (find "awesome.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             :find-new-host)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)

;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (process (getf msg :process)
;;                     (getf msg :process))
;;            (server (fdog-models:servers :name "forwarder" :refresh t :one t)
;;                    (fdog-models:servers :name "forwarder" :refresh t :one t)))
;;           ((and server (getf process :pid)
;;                 (equalp (getf process :pid)
;;                         (fdog-models:mongrel2-server-pid server)))
;;            :server-running)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :remove-host
;;                             :remove-host (:server "forwarder" :host "awesome.example.com"))))

;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :remove-host))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :removed-host)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1))))
;;            ((not (fdog-models:servers :name "forwarder" :refresh t :one t))
;;             :server-gone)))))

;; (def-test (mongrel2-agent-culls-servers :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :need-filled)
;;           (:eql :find-new-host)
;;           (:eql :removal-filled)
;;           (:eql :server-gone))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :need-filled)))

;;    (progn
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
;;             (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;        (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             :find-new-host)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :keep-servers
;;                             :keep-servers ("control"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :keep-servers))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :removal-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1))))
;;            ((not (fdog-models:servers :name "forwarder" :refresh t :one t))
;;             :server-gone)))))

;; (def-test (mongrel2-agent-culls-all-servers :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :need-filled)
;;           (:eql :find-new-host)
;;           (:eql :removal-filled)
;;           (:eql :servers-gone))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :need-filled)))

;;    (progn
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
;;             (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;        (and (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             :find-new-host)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :keep-servers
;;                             :keep-servers ())))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :keep-servers))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :removal-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1))))
;;            ((not (fdog-models:servers :refresh t))
;;             :servers-gone)))))

;; (def-test (mongrel2-agent-culls-hosts :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :need-filled)
;;           (:eql :find-new-host)
;;           (:eql :removal-filled)
;;           (:eql :hosts-gone))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com"
;;                                                                           "api2.example.com"
;;                                                                           "api3.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :need-filled)))

;;    (progn
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
;;             (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;        (and (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             (find "api3.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)
;;             :find-new-host)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :keep-hosts
;;                             :keep-hosts (:server "forwarder" :hosts ("api.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :keep-hosts))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled hosts: ~A" msg)
;;            :removal-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1)))
;;            (info (getf msg :info) (getf msg :info)))
;;           ((let* ((server (fdog-models:servers :name "forwarder" :refresh t :one t))
;;                   (hosts (and server (fdog-models:mongrel2-server-hosts server))))
;;              (and (not (find "api2.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name))
;;                   (not (find "api3.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name))
;;                   (find "api.example.com" hosts :test #'equalp :key #'fdog-models:mongrel2-host-name)))
;;            :hosts-gone)))))

;; (def-test (mongrel2-agent-can-make-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :server-need-filled)
;;           (:eql :server-found)
;;           (:eql :handler-need-filled)
;;           (:eql :host-found)
;;           (:eql :route-found)
;;           (:eql :handler-found)
;;           (:eql :handler-has-endpoint))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :server-need-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)

;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (process (getf msg :process)
;;                     (getf msg :process))
;;            (server (fdog-models:servers :name "forwarder" :refresh t :one t)
;;                    (fdog-models:servers :name "forwarder" :refresh t :one t)))
;;           ((and server (getf process :pid)
;;                 (equalp (getf process :pid)
;;                         (fdog-models:mongrel2-server-pid server)))
;;            :server-found)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :handler))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-need-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)

;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1)))
;;            (server (fdog-models:servers :name "forwarder" :refresh t :one t)
;;                    (fdog-models:servers :name "forwarder" :refresh t :one t)))
;;           ((and server
;;                 (fdog-models:find-mongrel2-host server "api.example.com"))
;;            :host-found)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)

;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1)))
;;            (server (fdog-models:servers :name "forwarder" :refresh t :one t)
;;                    (fdog-models:servers :name "forwarder" :refresh t :one t))
;;            (host (when server (fdog-models:find-mongrel2-host server "api.example.com"))
;;                  (when server (fdog-models:find-mongrel2-host server "api.example.com"))))
;;           ((and host
;;                 (fdog-models:find-mongrel2-route host "/"))
;;            :route-found)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)

;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1)))
;;            (server (fdog-models:servers :name "forwarder" :refresh t :one t)
;;                    (fdog-models:servers :name "forwarder" :refresh t :one t)))
;;           ((and server
;;                 (fdog-models:find-mongrel2-handler :ident "api" :exact nil))
;;            :handler-found)))

;;    (let* ((server (fdog-models:servers :one t :refresh t :name "forwarder"))
;;           (host (and server (fdog-models:find-mongrel2-host server "api.example.com")))
;;           (route (and host (fdog-models:find-mongrel2-route host "/")))
;;           (target (and route (fdog-models:mongrel2-route-target route))))
;;      (and target
;;           (fdog-models:mongrel2-handler-send-spec target)
;;           (fdog-models:mongrel2-handler-recv-spec target)
;;           (prog1 :handler-has-endpoint
;;             (log-for (trace mongrel2-agent::agent-needs) "Target: ~A" target))))))

;; (def-test (mongrel2-agent-announces-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :server-need-filled)
;;           (:eql :handler-need-filled)
;;           (:eql :handler-announced))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :server))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :server-need-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/admin/" :name "api-admin"))))
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :handler))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-need-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (do* ((msg (parse-message (read-message m :timeout 1))
;;                 (parse-message (read-message m :timeout 1)))
;;            (infop (equalp (car msg) :agent) (equalp (car msg) :agent))
;;            (info (getf msg :info) (getf msg :info))
;;            (provides (and infop (getf info :provides))
;;                      (and infop (getf info :provides)))
;;            (servers (getf provides :servers)
;;                     (getf provides :servers)))
;;           ((and provides servers)
;;            (let* ((server (assoc "forwarder" servers :test #'string=))
;;                   (server-info (cdr server))

;;                   (api (assoc "api" server-info :test #'string=))
;;                   (api-info (cdr api)))
;;              (and api-info
;;                   (getf api-info :send)
;;                   (getf api-info :recv)
;;                   :handler-announced)))))))

;; (def-test (mongrel2-agent-updates-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :handler-filled)
;;           (:eql :handler-refilled)
;;           (:eql :handler-updated))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (and (equalp (car msg) :filled) msg)))
;;           ((and filled
;;                 (getf filled :handler))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/magic/" :name "api"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :handler))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-refilled)))

;;    (progn
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :one t :refresh t :name "forwarder"))
;;             (host (and server (fdog-models:find-mongrel2-host server "api.example.com")))
;;             (old-route (and host (fdog-models:find-mongrel2-route host "/")))
;;             (route (and host (fdog-models:find-mongrel2-route host "/magic/")))
;;             (target (and route (fdog-models:mongrel2-route-target route))))
;;        (cond (old-route
;;               :old-route-exists)

;;              ((and route target)
;;               :handler-updated))))))

;; (def-test (mongrel2-agent-removes-handler :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :handler-filled)
;;           (:eql :handler-removed)
;;           (:eql :handler-missing))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (and (equalp (car msg) :filled) msg)))
;;           ((and filled
;;                 (getf filled :handler))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :remove-handler
;;                             :remove-handler (:server "forwarder" :name "api"))))
;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :remove-handler))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-removed)))

;;    (progn
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (let* ((server (fdog-models:servers :one t :refresh t :name "forwarder"))
;;             (host (and server (fdog-models:find-mongrel2-host server "api.example.com")))
;;             (old-route (and host (fdog-models:find-mongrel2-route host "/")))
;;             (old-handler (fdog-models:find-mongrel2-handler :ident "api" :exact nil)))
;;        (and (not old-route)
;;             (not old-handler)
;;             :handler-missing)))))

;; (def-test (mongrel2-agent-keeps-handlers :group mongrel2-agent-tests :fixtures (db-path-fixture mongrel2-agent-fixture kill-everything-fixture))
;;     (:seq (:eql :handler-filled)
;;           (:eql :handler-removed)
;;           (:eql :removed-missing)
;;           (:eql :kept-found))
;;   (list
;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :server
;;                             :server (:name "forwarder" :port 6969 :hosts ("api.example.com")))))
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/" :name "api"))))
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/2/" :name "api2"))))
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :handler
;;                             :handler (:server "forwarder" :hosts ("api.example.com") :route "/3/" :name "api3"))))

;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (and (equalp (car msg) :filled) msg)))
;;           ((and filled
;;                 (getf filled :handler)
;;                 (equalp (getf (getf filled :handler) :name) "api3"))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            :handler-filled)))

;;    (with-agent-conversation (m e) mongrel2-uuid
;;      (zmq:send! e (prepare-message
;;                    `(:agent :need
;;                             :need  :keep-handlers
;;                             :keep-handlers (:server "forwarder" :names ("api" "api3" "api4")))))

;;      (do* ((msg (parse-message (read-message m))
;;                 (parse-message (read-message m)))
;;            (filled (and (equalp (car msg) :filled) msg)
;;                    (or filled
;;                        (and (equalp (car msg) :filled) msg))))
;;           ((and filled
;;                 (getf filled :keep-handlers))
;;            (log-for (trace mongrel2-agent::agent-needs) "Filled: ~A" msg)
;;            (if (find "api4" (getf (getf filled :keep-handlers) :names) :test #'string=)
;;                :included-fake-name
;;                :handler-removed))))

;;    (progn
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (let* ((old-handler (fdog-models:find-mongrel2-handler :ident "api2" :exact nil)))
;;        (if old-handler
;;            :removed-exists
;;            :removed-missing)))

;;    (progn
;;      (ignore-errors (fdog-models:disconnect))
;;      (ignore-errors (clsql:disconnect))
;;      (fdog-models:connect db-path)
;;      (let* ((old-handler (fdog-models:find-mongrel2-handler :ident "api3" :exact nil)))
;;        (if old-handler
;;            :kept-found
;;            :kept-missing)))))
