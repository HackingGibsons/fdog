(defpackage #:api-app
  (:use :cl)
  (:use :log5)
  (:documentation "The HTTP application")

  (:use :http-dog)
  (:export :*name* :*description* :*version*
           :api))

(in-package :api-app)
