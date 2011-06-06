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

(defun run ()
  (when *verbose*
    (log-for (trace) "Running tests"))
  (nst-cmd :run-package :fdog-tests)

  (when *verbose*
    (log-for (trace) "Storing junit in ~A/~A" (asdf:system-source-directory :fdog-tests) "junit.xml"))
  (junit-results-by-group :dir (asdf:system-source-directory :fdog-tests)
                          :file "junit.xml"
                          :if-file-exists :supersede))
