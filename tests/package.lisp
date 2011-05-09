(defpackage #:fdog-tests
  (:use #:cl
        #:5am
        #:fdog
        #:fdog-models)
  (:shadowing-import-from :log5
                          :log-for))
(in-package :fdog-tests)


