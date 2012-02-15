(defpackage #:api-app
  (:use :cl)
  (:use :log5)
  (:documentation "The HTTP application")

  (:use :agent)
  (:use :http-dog)
  (:import-from :forwarder-agent
                :forwarder-to-alist)
  (:import-from :api-agent
                :api-agent
                :forwarders)
  (:export :*name* :*description* :*version*
           :api))

(in-package :api-app)
