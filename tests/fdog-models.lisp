(in-package :fdog-tests)

(def-suite fdog-models :description "fdog-models tests")
(in-suite fdog-models)

(test valid-endpoints-are-defined
      (let ((endpoints '(("proxy" . mongrel2-proxy)
                         ("handler" . mongrel2-handler)
                         ("dir" . mongrel2-directory))))
        (mapc (lambda (endpoint)
                (is (equal (fdog-models::endpoint-by-name (car endpoint)) (cdr endpoint)))) 
              endpoints)))