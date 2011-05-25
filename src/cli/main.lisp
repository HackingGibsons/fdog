(in-package :fdog-cli)

(defparameter *self* "fdog"
  "The name of the binary.")

;; Commands
(defcommand init (argv)
  "Initialize an installation given by a path."
  (with-cli-options (argv "Usage: init [options] [path]~%~@{~A~%~}~%")
      ((no-input "Don't prompt for input.") &free path)
    (let ((path (path-or-cwd path)))
      (format t "Initializing in: ~A~%" path)
      (setf path (ensure-directories-exist path :verbose t))

      (let ((dirs `(("server/" . ("logs/" "run/" "tmp/"))
                    ,*fdog-run-dirname*)))
        (labels ((create-dir (dir &key (base path))
                   (etypecase dir
                     (list (create-dir (car dir) :base base)
                           (mapcar (lambda (d) (create-dir d :base (merge-pathnames (car dir) base)))
                                   (cdr dir)))
                     (string (ensure-directories-exist (merge-pathnames dir base) :verbose t)))))
          (mapcar #'create-dir dirs)))

      (fdog-models:disconnect)
      (unwind-protect
           (let ((db-path (merge-pathnames (make-pathname :name "config" :type "sqlite"
                                                          :directory '(:relative "server"))
                                           (parse-namestring path)))
                 init)
             (if (probe-file db-path)
                 (setf init (or no-input
                                (yes-or-no-p "Server database exists, remove?")))
                 (setf init t))
             (and init
                  (probe-file db-path)
                  (delete-file db-path))
             (fdog-models:connect db-path)
             (when init
               (fdog-m2sh:init)
               (fdog-models:reconnect)
               (install-default-configuration)))
        (fdog-models:disconnect)))))

(defcommand start (argv)
  "Initialize an installation given by a path."
  (with-cli-options (argv "Usage: start [options] [path]~%~@{~A~%~}~%")
      (&free path)
    (let* ((path (path-or-cwd path))
           (db-path (fdog:make-fdog-server-db-pathname :root path))
           finished)
      (unless (probe-file db-path)
        (format t "ERROR: No configuration found at ~A~%" path)
        (quit :unix-status 1))
      (fdog:init :root path)

      (when (fdog-running-p)
          (format t "ERROR: This instance is already running!~%")
          (quit :unix-status 1))

      (fdog:start)

      (flet ((process-stop (&rest args)
               (declare (ignore args))
               (setf finished t)))
        (handler-case
            (cl-daemonize:daemonize :pid "/tmp/testproof.pid"
                                    :stop #'process-stop)
          (syscall-error (c)
            (format t "ERROR: I cannot daemonize on this platform, won't detach!~%")
            (sb-sys:enable-interrupt sb-posix:sigterm #'process-stop)
            (sb-sys:enable-interrupt sb-posix:sigint #'process-stop))))

      (loop do (sleep 0.25) (when finished
                              (format t "Terminating..~%")
                              (quit :unix-status 0))))))


(defcommand status (argv)
  "Determine the status of the fdog installation at the given path."
  (with-cli-options (argv "Usage: status [path]~%~@{~A~%~}~%")
      (&free path)
    (let* ((path (path-or-cwd path))
           (db-path (fdog:make-fdog-server-db-pathname :root path)))
      (unless (probe-file db-path)
        (format t "ERROR: No configuration found at ~A~%" path)
        (quit :unix-status 1))
      (fdog:init :root path)

      (format t "Status of fdog at ~A:~%" path)
      (if (fdog-running-p)
          (format t " Running: pid: ~A~%" (probe-fdog-pid))
          (format t " Not Running.~%"))
      (terpri)

      (let ((servers (fdog-m2sh:servers)))
        (when servers
          (format t "Mongrel2 Servers:~%"))
        (dolist (server servers servers)
          (format t "  Name: ~A  Status: ~:[not running~;running~]~%"
                  (fdog-models:mongrel2-server-name server)
                  (fdog-models:mongrel2-server-running-p server)))))))


(defcommand help (argv &key (exit 0))
  "Show help"
  (if argv
      (progn
        (format t "Help on command ~A:~%" (car argv))
        (let ((doc (get-command (car argv) :doc)))
          (when doc (format t "~A~%~%" doc)))
        (funcall (get-command (car argv) :function) '("-h")))
      (progn
        (format t "Usage: ~A <command> [command-options]~%" *self*)
        (list-commands)))
  (when exit (quit :unix-status 0)))

(defun list-commands ()
  (format t "Available commands:~%")
  (dolist (command *commands* *commands*)
    (format t "  ~A~%" (string-downcase (symbol-name (car command))))))

;; Commands and entries
(defun fdog-main (argv)
  ;; Don't want a CLI app dropping to a debugger repl
  (sb-ext:disable-debugger)

  (destructuring-bind (self &rest args) argv
    (setf *self* self)

    (unless args
      (funcall (get-command :help :function) nil))

    (let ((cmd (get-command (first args))))
      (if (not cmd)
          (progn
            (format t "~A is not a valid command.~%" (first args))
            (funcall (get-command :help :function) nil))
          (funcall (cdr cmd) (rest args))))))
