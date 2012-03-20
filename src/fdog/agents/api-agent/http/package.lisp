(defpackage :http-dog
  (:use :cl)
  (:use :log5
        :trivial-gray-streams)
  (:documentation "A package of HTTP helpers for afdog and m2cl")
  (:import-from :api-agent
                :api-agent)
  (:import-from :arnesi
                :awhen
                :it)
  (:import-from :alexandria
                :appendf)
  (:import-from :api-agent
                :api-mixin)
  (:export :chunked-http-output-stream
           :with-dispatch-on
           :with-chunked-stream-reply
           :header-json-type
           :decode-json-from-request
           :merge-headers
           :api-subpath
           :handle-http-condition
           :http-error-condition))

(in-package :http-dog)
