(in-package :afdog-tests)

(def-test (can-find-help-function :group cli-tests)
    (:all :true
          (:apply symbol-function (:predicate functionp)))
  (get-command :help :function))

(def-test (can-find-more-than-one-command :group cli-tests)
    (:apply length (:predicate (lambda (c) (> c 1))))
  afdog-cli::*commands*)


;; This test is kinda sad, since all you have to do to pass it is not
;; explode when passed -h..
(def-test (each-command-responds-to-help :group cli-tests)
    (:each (:apply
            (lambda (x)
              (if (equalp x "help")
                  :help-would-exit
                  (ignore-errors
                    (prog1 :pass (funcall (get-command x :function) '("-h"))))))
            :true))
  (mapcar #'symbol-name (mapcar #'car afdog-cli::*commands*)))

