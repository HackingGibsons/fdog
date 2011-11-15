(in-package :afdog-tests)

(def-test (can-find-help-function :group cli-tests)
    (:all :true
          (:apply symbol-function (:predicate functionp)))
  (get-command :help :function))

(def-test (can-find-more-than-one-command :group cli-tests)
    (:apply length (:predicate (lambda (c) (> c 1))))
  afdog-cli::*commands*)

