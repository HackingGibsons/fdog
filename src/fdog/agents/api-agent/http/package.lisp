(defpackage :http-dog
  (:use :cl)
  (:use :log5
        :trivial-gray-streams)
  (:documentation "A package of HTTP helpers for afdog and m2cl")
  (:export :chunked-http-output-stream
           :with-dispatch-on
           :with-chunked-stream-reply
           :header-json-type
           :merge-headers
           :api-subpath
           :handle-http-condition
           :http-error-condition))

(in-package :http-dog)
