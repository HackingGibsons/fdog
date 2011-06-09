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

(defun unpack-to (dir)
  (unless (ppcre:scan "/$" dir)
    (setf dir (concatenate 'string dir "/")))
  (ensure-directories-exist dir :verbose t)
  (format t "Unpack to: ~A~%" (truename dir))
  (let ((bundle-file (with-open-file (temp-file (format nil "/tmp/fdog.bundle.~A.tgz" (random 10000))
                                                :direction :output :element-type 'flex:octet
                                                :if-exists :supersede)
                       (write-sequence *data* temp-file)
                       (pathname temp-file))))
    (format t "Wrote bundle to: ~A~%" bundle-file)
    (external-program:run "tar" `("zx" "-C" ,dir "-f" ,(namestring bundle-file)))))

(defun bundle (argv)
  (destructuring-bind (self &rest args) argv
    (with-cli-options (args t)
        (&free out)
      (unless (= 1 (length out))
        (format *error-output* "You must specify an output path~%")
        (quit :unix-status 1))
      (unpack-to (car out)))))
