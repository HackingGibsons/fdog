(in-package :http-dog)

(defun header-json-type ()
  '("Content-Type" . "application/json"))

(defun merge-headers (headers)
  (let ((default '((:code . 200) (:status . "OK")
                   ("Content-Type" . "text/html")
                   ("X-Fdog" . "afdog"))))
    (remove-duplicates (append default (remove-if #'null headers :key #'cdr))
                       :key #'car :test #'string=)))
