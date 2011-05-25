(in-package :fdog-cli)

(defparameter *self* "fdog"
  "The name of the binary.")

;; Commands
(defcommand init (argv)
  "Initialize an installation given by a path."
  (with-cli-options (argv "Usage: init [path]~%~@{~A~%~}~%")
      (&free path)
    (let* ((path (or (car path)
                     (getcwd)))
           (path (if (ppcre:scan "/$" path) path (format nil "~A/" path))))
      (format t "Initializing in: ~A~%" path)
      (setf path (ensure-directories-exist path :verbose t))

      (let ((dirs '(("server/" . ("logs/" "run/" "tmp/"))
                    "run/")))
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
                 (setf init (yes-or-no-p "Server database exists, remove?"))
                 (setf init t))
             (and init
                  (probe-file db-path)
                  (delete-file db-path))
             (fdog-models:connect db-path)
             (when init
               (fdog-m2sh:init)))
        (fdog-models:disconnect)))))

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
