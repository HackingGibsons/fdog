;;;; fdog.lisp

(in-package #:fdog)

(defun make-fdog-server-db-pathname (&key (root *default-root-path*)
                                          (server *default-server-path*)
                                          (database *default-server-database-path*))
  (reduce #'merge-pathnames (list database server root)))

;;; Init
(defun init (&key (root *default-root-path*)
                  (server *default-server-path*)
                  (database *default-server-database-path*))
  "Initialization function for fdog
Should find and assert the correctness of the project root, server dir, and then connect to the server database"
  (setf *root-path* root)
  (fdog-models:connect (make-fdog-server-db-pathname :root root :server server :database database)))

(defmethod start ()
  "Start the fdog daemon and run the control interface"
  (if (not (fdog-models:connected-p))
      (error "Cannot start, not initialized."))
  (fdog-control:init-control-interface))

(defun probe-fdog-pidfile (&optional path)
  "Probe for the pid of the current initialized fdog instance,
or seek it from the path"
  (let* ((path (merge-pathnames *fdog-run-dirname* (or path *root-path*)))
         (path (merge-pathnames *fdog-master-pidfile* path)))
    (probe-file path)))

(defun probe-fdog-pid (&optional path)
  "Given a path, it will attempt to find a PIDfile then
try to read a pid out of it."
  (let ((pidfile (probe-fdog-pidfile path)))
    (when pidfile
      (with-open-file (s pidfile) (read s)))))

(defun fdog-running-p (&optional path)
  (let* ((pid (probe-fdog-pid path))
         (running (when pid
                    (handler-case (kill pid 0)
                      (syscall-error () nil)))))
    running))
