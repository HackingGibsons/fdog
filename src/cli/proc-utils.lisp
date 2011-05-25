(in-package :fdog-cli)

(defun probe-fdog-pidfile (&optional path)
  "Probe for the pid of the current initialized fdog instance,
or seek it from the path"
  (let* ((path (merge-pathnames *fdog-run-dirname* (or path fdog:*root-path*)))
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

