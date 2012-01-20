(defpackage #:afdog-cli
  (:use #:cl)
  (:use :afdog)
  (:use :agent)
  (:use :fdog)
  (:use :unix-options)

  #+sbcl
  (:shadowing-import-from :sb-ext quit)

  (:export :main
           :*self*
           :defcommand
           :get-command))

(in-package :afdog-cli)

(defvar *self* "afdog" "The name of the binary.")
(defvar *agent-packages* '(:agent :fdog) "The packages to search for agent classes")
(defvar *agent-spawner* :cli "The spawner to set the default to.")

(defgeneric main (argv)
  (:documentation "The main entry point to the CLI interface of the application.")
  (:method :before (argv)
           ;; Without this we would get reliably random numbers in saved cores
           ;; but not otherwise. And by reliably random, I mean identical sequences between
           ;; execution. The bad kind.
           (setf *random-state* (make-random-state t)
                 ;; And isn't it nice of uuid to both keep a separate random state
                 ;; and to make it private?
                 uuid::*uuid-random-state* (make-random-state t))

           #+sbcl (sb-ext:disable-debugger))

  (:method :around (argv)
           (sb-sys:enable-interrupt sb-posix:sigint
                                    #'(lambda (&rest args)
                                        (format t "~&Interrupted!~%")
                                        (sb-ext:quit :unix-status sb-posix:sigint)))
           (let ((*spawner* *agent-spawner*))
             (call-next-method))

           (sb-sys:default-interrupt sb-posix:sigint))

  (:method (argv)
    (destructuring-bind (self &rest args) argv
      (setf *self* (or (iolib.syscalls:getenv "AFDOG") self))

      (unless args
        (funcall (get-command :help :function) nil))

      (let ((cmd (get-command (first args))))
        (if (not cmd)
            (progn
              (format t "~A is not a valid command.~%" (first args))
              (funcall (get-command :help :function) nil))
            (funcall (cdr cmd) (rest args)))))))
