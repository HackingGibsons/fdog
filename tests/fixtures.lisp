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
      (fdog-m2sh:with-server ("testing" :bind "127.0.0.1" :port 7357 :chroot "./")
        (fdog-m2sh:with-host ("localhost"))))
     ,@body))

(defmacro +m2/with-server (&body body)
  `(let ((server (car (clsql:select 'mongrel2-server :flatp t :refresh t))))
     ,@body))

;;; Foxtures/Mixtures
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
