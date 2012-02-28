(defpackage #:api-app
  (:use :cl)
  (:use :log5)
  (:documentation "The HTTP application")

  (:use :agent)
  (:use :http-dog)
  (:import-from :alexandria
                :appendf)
  (:import-from :api-agent
                :api-agent
                :forwarders
                :handlers
                :callback
                :register-callback)
  (:import-from :forwarder-agent
                :handler-name)
  (:export :*name* :*description* :*version*
           :api))

(in-package :api-app)
