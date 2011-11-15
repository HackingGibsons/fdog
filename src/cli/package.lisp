(defpackage #:afdog-cli
  (:use #:cl)
  (:use :afdog)
  (:use :agent)
  (:use :unix-options)

  #+sbcl
  (:shadowing-import-from :sb-ext quit)

  (:export :main
           :*self*
           :defcommand
           :get-command))

(in-package :afdog-cli)

(defvar *self* "afdog" "The name of the binary.")
(defvar *agent-packages* '(:agent) "The packages to search for agent classes")
(defvar *agent-spawner* :cli "The spawner to set the default to.")

(defgeneric main (argv)
  (:documentation "The main entry point to the CLI interface of the application.")
  (:method :before (argv)
           #+sbcl (sb-ext:disable-debugger))

  (:method :around (argv)
           (let ((*spawner* *agent-spawner*))
             (call-next-method)))

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
