(in-package :fdog-tests)
;; Helpers
(defmacro def-test+fdog (name &body body)
  "Utility to shorten the process of writing a mongrel2 db test"
  `(def-test (,name :group fdog-forwarder-api-tests
                    :fixtures ())
       ,@body))


;; Tests
(def-test+fdog can-find-servers
  :true (fdog-m2sh:servers))
