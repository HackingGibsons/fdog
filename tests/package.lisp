(defpackage #:afdog-tests
  (:use #:cl
        #:afdog
        #:agent-host
        #:agent
        #:fdog
        #:json
        #:afdog-cli)
  (:use #:nst)
  (:import-from :arnesi
                :it
                :curry
                :rcurry
                :awhen
                :aand
                :when-bind)

  (:import-from :forwarder-agent
                :*forwarder-server*
                :*forwarder-filename*)

  (:import-from :request-processing-agent
                :request-handler
                :agent-requesticle)

  (:import-from :api-agent
                :api-agent)

  (:import-from :request-forwarder-agent
                :request-forwarder-agent
                :forwarder-endpoint
                :push-state
                :name
                :push-state-signal)

  (:shadow :*spawner* :*root* :agent-root)
  (:shadowing-import-from :log5
                          :defcategory
                          :log-for)
  (:export :run-all))

(in-package :afdog-tests)

(defvar *spawner* :host
  "Use the :host spawner in the :afdog-tests package
with the :test spawner used to initially start agents")

(defvar *root* (merge-pathnames (make-pathname :directory '(:relative "tests"))
                                (asdf:system-source-directory :afdog-tests))
  "Root for test runs")

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
