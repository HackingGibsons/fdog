(in-package :fdog-tests)
;; Helpers
(defmacro def-test+func (name &body body)
  "Utility to shorten the process of writing an fdog functional test"
  `(def-test (,name :group fdog-forwarder-functional-tests
                    :fixtures (fdog/functional))
       ,@body))

(def-test+func can-hit-slash :eval
  (let* ((url (format nil "http://~A:~A/" "localhost" 1337))
         (res (http->string url)))
    (assert-non-nil res)))


