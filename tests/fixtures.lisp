(in-package :fdog-tests)

;; NST
(def-fixtures database/connected
    (:setup (progn
              (log-for (trace) "DB Connected setup")
              (fdog-models:disconnect) ;; Let's make sure we don't trash the flow of data in a testrun
              (fdog-models:connect db-path))
     :cleanup (progn
                (fdog-models:disconnect)
                (log-for (trace) "DB Disconnected")
                (delete-file db-path)))

  ;; Bindings
  (base-db-path (reduce #'merge-pathnames (list *default-server-path* *default-root-path*)))
  (base-db-name (namestring base-db-path))
  (db-name (make-pathname :name "test" :type "sqlite"))
  (db-path (merge-pathnames db-name base-db-path))
  (*default-server-database-path* db-name))

(def-fixtures database/inited
    (:setup (fdog-m2sh:init)))


(def-fixtures database/configured
    (:startup (progn
              (log-for (trace) "DB configured startup")
              (fdog-m2sh:using-configuration!
               (fdog-m2sh:with-server (+server-name+ :bind +server-bind+ :port +server-port+ :chroot "./")
                 (fdog-m2sh:with-host ("localhost")
                   (fdog-m2sh:make-route "/static/" (fdog-m2sh:make-dir "./tests/")))))
              (log-for (trace) "Servers loaded: ~A" (fdog-m2sh:servers :one t :name +server-name+)))
     :finish (log-for (trace) "DB configured finish"))

  (server (fdog-m2sh:servers :name +server-name+ :refresh t :one t)))

(def-fixtures mongrel2/running
    (:setup (progn
              (log-for (trace) "Mongrel2 setup")
              (if (eql :timeout (progn (mongrel2-server-signal/block server :stop)
                                       (mongrel2-server-signal/block server :start)))
                  (error "Server spawn has timed out")
                  (log-for (trace) "Mongrel2 started: ~A" (mongrel2-server-pid server))))
     :cleanup (progn
                (mongrel2-server-signal/block server :stop)
                (log-for (trace) "Mongrel2 stopped"))))
