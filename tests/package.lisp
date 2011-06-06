(defpackage #:fdog-tests
  (:use #:cl
        #:fdog
        #:fdog-models)
  (:use #:nst)
  (:shadowing-import-from :log5
                          :log-for)
  (:export :*verbose*))
(in-package :fdog-tests)

(defvar +server-name+ "testing")
(defvar +server-bind+ "127.0.0.1")
(defvar +server-port+ 7357)
(defvar +default-host+ "localhost")

(defvar *verbose* t)

(defun run ()
  (format t "Running tests.~%")
  (nst-cmd :run-package :fdog-tests)
  (junit-results-by-group :dir "." :file "junit.xml" :if-dir-does-not-exist t :if-file-exists :supersede))

;; (defmethod 5am::%run :around (test-spec)
;;   (when *verbose*
;;     (log-for (dribble) "About to run test: ~S" test-spec))

;;   (let ((result (call-next-method)))
;;     (when *verbose*
;;       (log-for (dribble) "Finished running test: ~S" test-spec))
;;     result))
