(defpackage #:api-app
  (:use :cl)
  (:use :log5)
  (:documentation "The HTTP application")

  (:use :http-dog)
  (:export :api))

(in-package :api-app)
