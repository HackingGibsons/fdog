(in-package :afdog-cli)

(defclass cli-runner (agent::exec-runner) nil
  (:default-initargs .
      (:lisp *self* :options "repl"
             :init nil
             :exec-prefix nil :terminate nil))

  (:documentation
   "A subclass of `agent::exec-runner' that uses
the repl command of the command line."))
