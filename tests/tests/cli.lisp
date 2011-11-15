(in-package :afdog-tests)

(def-test (can-find-help-function :group cli-tests)
    (:all :true
          (:apply symbol-function (:predicate functionp)))
  (get-command :help :function))

