(defpackage #:fdog-tests
  (:use #:cl
        #:fdog
        #:fdog-models)
  (:use #:nst)
  (:shadowing-import-from :log5
                          :log-for)
  (:export :*verbose*
           :run))
(in-package :fdog-tests)

(defvar +server-name+ "testing")
(defvar +server-bind+ "127.0.0.1")
(defvar +server-port+ 7357)
(defvar +default-host+ "localhost")

(defvar *verbose* t)

(defun run-functional ()
  (let ((results-dir (merge-pathnames (make-pathname :directory '(:relative "tests" "results"))
                                      (asdf:system-source-directory :fdog-tests))))
    (when *verbose*
      (log-for (trace) "Running functional tests"))

    (nst-cmd :run-group functional-tests)

    (when *verbose*
      (log-for (trace) "Storing junit in ~A" results-dir))
    (junit-results-by-group :dir results-dir
                            :if-file-exists :supersede
                            :if-dir-does-not-exist :create)))

(defun run-unit ()
  (let ((results-dir (merge-pathnames (make-pathname :directory '(:relative "tests" "results"))
                                      (asdf:system-source-directory :fdog-tests))))
    (when *verbose*
      (log-for (trace) "Running tests"))

    (nst-cmd :run-group unit-tests)

    (when *verbose*
      (log-for (trace) "Storing junit in ~A" results-dir))
    (junit-results-by-group :dir results-dir
                            :if-file-exists :supersede
                            :if-dir-does-not-exist :create)))

(defun run-all ()
  (let ((results-dir (merge-pathnames (make-pathname :directory '(:relative "tests" "results"))
                                      (asdf:system-source-directory :fdog-tests))))
    (when *verbose*
      (log-for (trace) "Running tests"))

    (nst-cmd :run-group all-tests)

    (when *verbose*
      (log-for (trace) "Storing junit in ~A" results-dir))
    (junit-results-by-group :dir results-dir
                            :if-file-exists :supersede
                            :if-dir-does-not-exist :create)))

