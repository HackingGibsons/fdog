(defpackage #:fdog-cli
  (:use #:cl)
  (:export :fdog-main)
  (:shadowing-import-from :sb-ext
                          :quit)
  (:shadowing-import-from :log5
                          :log-for :info :warn :error))
(in-package :fdog-cli)
