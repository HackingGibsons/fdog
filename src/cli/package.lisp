(defpackage #:fdog-cli
  (:use #:cl
        #:unix-options)
  (:export :fdog-main)
  (:shadowing-import-from :fdog-m2sh
                          :using-configuration!
                          :with-server :with-host
                          :make-route :make-handler :make-dir :make-proxy)
  (:shadowing-import-from :sb-ext
                          :quit)
  (:shadowing-import-from :sb-posix
                          :getcwd
                          :syscall-error
                          :kill :sigint :sigterm :sighup)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error))
(in-package :fdog-cli)

(defparameter *fdog-run-dirname* "run/")
(defparameter *fdog-master-pidfile* "fdog.master.pid")
