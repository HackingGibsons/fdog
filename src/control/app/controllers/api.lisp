(in-package :fdog-control)

(defparameter *api-version* 1)

(defun api/root (handler request raw)
  (declare (ignorable raw))
  (let* ((prefix "/api")
         (sub-path (ppcre:regex-replace (format nil "^~A" prefix)
                                        (m2cl:request-path request)
                                        "")))
    (log-for (trace) "API request for ~A" sub-path))
  (with-chunked-stream-reply (handler request stream
                              :headers ('("Content-Type" . "application/json")))
    (json:encode-json '((:hello . :world)) stream)))
