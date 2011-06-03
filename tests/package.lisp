(defpackage #:fdog-tests
  (:use #:cl
        #:5am
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

(defmethod 5am::%run :around (test-spec)
  (when *verbose*
    (log-for (dribble) "About to run test: ~S" test-spec))

  (let ((result (call-next-method)))
    (when *verbose*
      (log-for (dribble) "Finished running test: ~S" test-spec))
    result))
