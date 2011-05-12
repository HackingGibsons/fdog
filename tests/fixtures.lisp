(in-package :fdog-tests)

;;; Mixin macros here
(defmacro +db/connected (&body body)
  (let* ((base-db-path (reduce #'merge-pathnames (list *default-server-path* *default-root-path*)))
         (base-db-name (namestring base-db-path)))
    `(let* ((db-name (make-pathname :name "test" :type "sqlite"))
            (db-path (merge-pathnames db-name ,base-db-path))
            (*default-server-database-path* db-name))
       (fdog-models:connect db-path)

       ,@body

       (fdog-models:disconnect)
       (delete-file db-path))))

(defmacro +db/inited (&body body)
  `(progn
     (fdog-m2sh:init)
     ,@body))

(defmacro +db/configured (&body body)
  `(progn
     (fdog-m2sh:using-configuration!
      (fdog-m2sh:with-server (+server-name+ :bind +server-bind+ :port +server-port+ :chroot "./")
        (fdog-m2sh:with-host ("localhost")
          (fdog-m2sh:make-route "/static/" (fdog-m2sh:make-dir "./tests/")))))
     ,@body))

(defmacro +m2/with-server (&body body)
  `(let ((server (car (clsql:select 'mongrel2-server :flatp t :refresh t))))
     ,@body))

(defmacro +m2/with-default-host (&body body)
  `(let ((default-host (mongrel2-server-default-host server)))
     ,@body))

(defmacro +m2/running (&body body)
  `(progn
     (if (eql :timeout (progn (mongrel2-server-signal/block server :stop)
                              (mongrel2-server-signal/block server :start)))
         (skip "Server spawn has timed out")
         (progn ,@body))
     (mongrel2-server-signal/block server :stop)))

;;; Fixtures/Mixtures
(def-mixture db/connected ()
    (+db/connected)
  (&body))

(def-mixture db/inited ()
    (+db/connected +db/inited)
  (&body))

(def-mixture db/configured ()
    (+db/connected +db/inited +db/configured)
  (&body))

(def-mixture m2/with-server ()
    (+db/connected +db/inited +db/configured +m2/with-server)
  (&body))

(def-mixture m2/with-server+default-host ()
    (+db/connected +db/inited +db/configured +m2/with-server +m2/with-default-host)
  (&body))


(def-mixture m2/with-running-server ()
    (+db/connected +db/inited +db/configured +m2/with-server +m2/running)
  (&body))
