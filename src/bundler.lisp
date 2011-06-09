(use-package :unix-options)

(defvar *data* nil)

(defun read-data-from (path)
  "Fill in the `*data*' with the bundle data.
Should be called during build"
  (setf *data*
        (flex:with-output-to-sequence (s)
          (with-open-file (in path :element-type 'flex:octet)
            (do ((x (read-byte in nil in) (read-byte in nil in)))
                ((eq x in))
              (write-byte x s))))))

(defun bundle (argv)
  (with-cli-options (argv)
      ()
    (format t "Le bundle: ~A~%" argv)
    (format t "Have data: ~A~%" (length *data*))))
