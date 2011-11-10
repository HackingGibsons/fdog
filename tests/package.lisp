(defpackage #:afdog-tests
  (:use #:cl
        #:afdog
        #:agent)
  (:use #:nst)

  (:shadow :*spawner*)
  (:shadowing-import-from :log5
                          :defcategory
                          :log-for)
  (:export :run-all))

(in-package :afdog-tests)

(defvar *spawner* :test
  "Use the :test spawner in the :afdog-tests package")

(defun run-all ()
  (let ((results-dir (merge-pathnames (make-pathname :directory '(:relative "tests" "results"))
                                      (asdf:system-source-directory :afdog-tests))))

    (format t "Storing junit in ~A~%" results-dir)
    (ensure-directories-exist results-dir :verbose t)

    (nst-cmd :run-group all-tests)

    (junit-results-by-group :dir results-dir
                            :if-file-exists :supersede
                            :if-dir-does-not-exist :create)))

(defcategory test)
