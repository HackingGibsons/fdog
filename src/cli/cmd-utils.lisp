(in-package :fdog-cli)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *commands* ()
    "Command mappings in the form:
 ((command-name-sym . command-func-sym)..)"))

;; Command definition helpers
(defmacro defcommand (name args &body body)
  "Create a function named fdog-cmd-`name' and add
an entry to the `*commands*' table"
  (let* ((title name)
        (prefix "fdog-cmd-")
        (cmd-name (intern (string-upcase (concatenate 'string prefix (symbol-name title))))))


    `(progn
       (pushnew '(,title . ,cmd-name) *commands* :test #'(lambda (a b) (and (eql (car a) (car b))
                                                                            (eql (cdr a) (cdr b)))))
       (defun ,cmd-name ,args
         ,@body))))

(defun get-command (name &optional (what :both))
  (let ((cmd (assoc name *commands* :test #'string-equal)))
    (and cmd
         (ecase what
           (:both cmd)
           (:name (car cmd))
           (:function (cdr cmd))
           (:doc (documentation (get-command name :function) 'function))))))


(defun path-or-cwd (maybe-path)
  (let* ((path (or (if (listp maybe-path)
                       (car maybe-path)
                       maybe-path)
                   (getcwd)))
         (path (if (ppcre:scan "/$" path) path (format nil "~A/" path))))
    path))
