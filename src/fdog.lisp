;;;; fdog.lisp
(in-package #:fdog)

;; Logging
(start-logging)



(defun make-fdog-server-db-pathname (&key (root *default-root-path*)
                                          (server *default-server-path*)
                                          (database *default-server-database-path*))
  (reduce #'merge-pathnames (list database server root)))

;;; Init
(defun init (&key (root *default-root-path*) (server *default-server-path*) (database *default-server-database-path*))
  "Initialization function for fdog
Should find and assert the correctness of the project root, server dir, and then connect to the server database"
  (setf *root-path* root)

  (log-for (trace) "Computing the local address.")
  (compute-local-address)
  (log-for (trace) "Found local address to be: ~A" (get-local-address :as :string))

  (fdog-models:connect (make-fdog-server-db-pathname :root root :server server :database database)))

(defmethod start ()
  "Start the fdog daemon and run the control interface"
  (if (not (fdog-models:connected-p))
      (error "Cannot start, not initialized."))
  (when (fdog-running-p)
    (error "Cannot start, already running."))

  (log-for (trace) "Wrote a pidfile(~A) to: ~A" (sb-posix:getpid) (fdog-pidfile-path))
  (with-open-file (out (fdog-pidfile-path)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~A" (sb-posix:getpid)))

  ;; Nuke the pidfile on exit
  (push (lambda ()
          (and (probe-fdog-pidfile)
               (delete-file (fdog-pidfile-path))))
        sb-ext:*exit-hooks*)

  (fdog-services:init-services))

(defun fdog-pidfile-path (&optional path)
  "Return where the pidfile should be for either the given fdog path
or the current connected path."
  (let* ((path (merge-pathnames *fdog-run-dirname* (or path *root-path*)))
         (path (merge-pathnames *fdog-master-pidfile* path)))
    path))

(defun probe-fdog-pidfile (&optional path)
  "Probe for the pid of the current initialized fdog instance,
or seek it from the path"
  (probe-file (fdog-pidfile-path path)))

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
