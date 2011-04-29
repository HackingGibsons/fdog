(defpackage #:fdog-tests
  (:use #:cl
        #:it.bese.FiveAM
        #:fdog-models))

(in-package :fdog-tests)
(run! 'fdog-models)