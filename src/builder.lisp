;; Help image reloading find the correct libraries
(with-unlocked-packages (:sb-alien)
  (let ((function (symbol-function 'sb-alien::try-reopen-shared-object)))
    (setf (symbol-function 'sb-alien::try-reopen-shared-object)
          #'(lambda (obj)
              (declare (type sb-alien::shared-object obj))
              (let ((path (sb-alien::shared-object-pathname obj)))
                (when (pathname-directory path)
                  (unless (probe-file path)
                      (let ((sub-path (make-pathname :name (pathname-name path)
                                                     :type (pathname-type path))))
                        (setf (sb-alien::shared-object-pathname obj) sub-path
                              (sb-alien::shared-object-namestring obj) (namestring sub-path))))))
              (funcall function obj)))))
