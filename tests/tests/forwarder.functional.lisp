(in-package :fdog-tests)
;; Helpers
(defmacro def-test+func (name &body body)
  "Utility to shorten the process of writing an fdog functional test"
  `(def-test (,name :group fdog-forwarder-functional-tests
                    :fixtures ())
       ,@body))

(def-test+func can-hit-slash :eval
  (let* ((url (format nil "http://~A:~A/" "localhost" 1337))
         (res (bt:with-timeout (3)
                (handler-case (http->string url)
                    (bt:timeout () nil)))))
    (assert-non-nil res)))


