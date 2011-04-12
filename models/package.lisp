(defpackage :fdog-models
  (:use :cl)
  (:export :connected-p :connect :disconnect :reconnect
           :*server-database*

           ;; Mongrel2 Configuration Classes
           :mongrel2-server
           :mongrel2-host
           :mongrel2-handler))
(in-package :fdog-models)

;; TODO: This may be better expressed in CLOS, now that I've noticed what I'm doing..
;;   Re: Keeping state and a pile of methods to poke it

;; Knobs
(defvar *server-database* nil
  "A handle to the database connection to the server.")

;; Methods
(defun connected-p ()
  "Bool of the current connection state"
  (and *server-database* (member *server-database* (clsql:connected-databases))))

(defun connect (&optional db-path)
  "Connect to a mongrel2 configuration database.
If `db-path' is omitted a default is used composed of:
  `fdog:*default-server-database-path*' `fdog:*default-server-path*' `fdog:*default-root-path*'
Merged into a pathname."
  (let* ((paths (list fdog:*default-server-database-path* fdog:*default-server-path* fdog:*default-root-path*))
         (db-path (or db-path (reduce #'merge-pathnames paths))))
    (format t "Paths: ~A~%" db-path)
    (unless (connected-p)
      (setf *server-database* (clsql:connect `(,(namestring db-path)) :database-type :sqlite3)))))

(defun disconnect ()
  "Disconnect from any existing database connecting we have.
Return t if an action took place, nil otherwise"
  (when (connected-p)
    (clsql:disconnect :database *server-database*))
  (when *server-database*
    (not (setf *server-database* nil))))

(defun reconnect ()
  "Disconnect and reconnect to the database
Returns two values, the result of disconnecting and connecting"
  (let ((name (and (connected-p) (clsql:database-name *server-database*))))
    (values (disconnect)
            (connect name))))
