(defpackage #:afdog-tests
  (:use #:cl
        #:afdog
        #:agent)
  (:use #:nst)
  (:shadowing-import-from :log5
                          :log-for)
  (:export :run-all))

(in-package :afdog-tests)

(defun run-all ()
  (let ((results-dir (merge-pathnames (make-pathname :directory '(:relative "tests" "results"))
                                      (asdf:system-source-directory :afdog-tests))))
    (nst-cmd :run-group all-tests)

    (log-for (trace) "Storing junit in ~A" results-dir))
    (junit-results-by-group :dir results-dir
                            :if-file-exists :supersede
                            :if-dir-does-not-exist :create))

