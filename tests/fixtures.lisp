(in-package :fdog-tests)

;;; Mixin macros here
(defmacro +db/connected (&body body)
  (let* ((base-db-path (reduce #'merge-pathnames (list *default-server-path* *default-root-path*)))
         (base-db-name (namestring base-db-path)))
    `(let ((db-path (make-pathname :directory '(:absolute ,base-db-name)
                                   :name "test" :type "sqlite")))
       (fdog-models:connect db-path)

       ,@body

       (fdog-models:disconnect)
       (delete-file db-path))))

(defmacro +db/configured (&body body)
  `(progn
     (fdog-m2sh:init)
     ,@body))


;;; Foxtures/Mixtures
(def-mixture db/connected ()
    (+db/connected)
  (&body))

(def-mixture db/configured ()
    (+db/connected +db/configured)
  (&body))
