(defpackage :http-dog
  (:use :cl)
  (:use :log5
        :trivial-gray-streams)
  (:documentation "A package of HTTP helpers for afdog and m2cl")
  (:export :chunked-http-output-stream
           :with-dispatch-on
           :with-chunked-stream-reply
           :merge-headers))

(in-package :http-dog)
