(in-package :afdog-cli)

(defclass cli-runner (exec-runner) nil
  (:default-initargs .
      (:lisp *self* :options '("repl")
             :init nil
             :exec-prefix nil :terminate nil))

  (:documentation
   "A subclass of `agent::exec-runner' that uses
the repl command of the command line."))

(defmethod make-runner ((style (eql :cli)) &rest initargs &key class &allow-other-keys)
  ;; COPY COPY PASTA HOORAY :( (straight from the :exec runner)
  (labels ((remove-from-plist (list key &rest keys)
             (reduce #'(lambda (acc b) (remove-from-plist acc b)) keys
                     :initial-value (cond ((null list) nil)
                                          ((equalp (car list) key) (remove-from-plist (cddr list) key))
                                          (t (append (list (first list) (second list)) (remove-from-plist (cddr list) key)))))))
    ;; TODO: Make this not make me want to gouge my eyes out.
    (make-instance 'cli-runner :agent class
                   :initargs (remove-from-plist initargs :class :agent :include))))
