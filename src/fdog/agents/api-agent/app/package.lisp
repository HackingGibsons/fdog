(defpackage #:api-app
  (:use :cl)
  (:use :log5)
  (:documentation "The HTTP application")

  (:use :http-dog)
  (:import-from :api-agent
                :api-agent)
  (:export :*name* :*description* :*version*
           :api))

(in-package :api-app)
