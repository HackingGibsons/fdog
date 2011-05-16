(defpackage :fdog-control
  (:use :sb-mop)
  (:use :cl :fdog-models :bordeaux-threads :fdog-handler)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defvar *interface-bridges* ()
  "Bridges configured as part of the web UI")

(defclass fdog-interface ()
  ((server :initarg :server
           :initform "control"
           :accessor fdog-interface-server)
   (bridges :initform ()
            :accessor fdog-interface-bridges)
   (routes :initform ()
           :accessor fdog-interface-routes))
  (:documentation "An interface for interacting with fdog through Mongrel2"))

(defmethod initialize-instance :after ((self fdog-interface) &rest initargs)
  (declare (ignore initargs))
  (with-slots (server) self
    (unless (typep server 'mongrel2-server)
      (let ((servers (fdog-m2sh:servers :name server :refresh t)))
        (if servers
            (setf server (car servers))
            (error (format nil "Could not initialize server from value: ~A" server))))))
  (initialize-interface self))

(defmethod initialize-interface ((self fdog-interface))
  (with-slots (server bridges routes) self
    (mapcar #'request-handler-stop bridges)
    (setf bridges ()
          routes ())

    (dolist (route (mongrel2-host-routes (mongrel2-server-default-host server)) self)
      (pushnew route routes)
      (with-accessors ((target mongrel2-route-target)) route
        (typecase target
          (mongrel2-handler
           (pushnew (configure-bridges-for target) bridges))
          (otherwise
           (unless target
             (log-for (warn) "NIL TARGET FOR ROUTE ~A(~A)" route (slot-value route 'fdog-models::id)))))))))


;;; Scaffold
;; Some varsdefs
(defun scaffold ()
  (defparameter *server* (car (fdog-m2sh:servers :refresh t)))
  (defparameter *host* (mongrel2-server-default-host *server*))
  (defparameter *routes* (mongrel2-host-routes *host*))
  (defparameter *handler* (car (remove-if-not (lambda (tr) (typep tr 'mongrel2-handler))
                                              (mapcar #'mongrel2-route-target *routes*))))
  (defparameter *dir* (car (remove-if-not (lambda (tr) (typep tr 'mongrel2-directory))
                                          (mapcar #'mongrel2-route-target *routes*))))


  (defun response (r)
    (format nil "~A:~A" (get-universal-time) (current-thread)))

  (defparameter *control-handler*
    (let ((handler (configure-bridges-for *handler*)))
      (request-handler-add-string-responder handler 'response)
      handler))
  ;; Chunky
  (defun chunk-info/start (request)
    (declare (ignorable request))
    '((:X-hello-world . "I am awesome")
      ("Trailer" . "X-Magic")))

  (defun chunk-trailer (request)
    `(("X-Magic" . "Totally")))

  (defun chunk-two (request)
    (declare (ignorable request))
    (values "  Verily, this is the second reply!"
            (not (= (random 10) 0))))

  (defparameter *chunked-handler*
    (let ((handler (configure-bridges-for *handler*)))
      (request-handler-add-chunked/trailer handler 'chunk-trailer)
      (request-handler-add-chunked/stop handler)

      (request-handler-add-chunked/chunk handler 'chunk-two)
      (request-handler-add-chunked/chunk handler 'response)

      (request-handler-add-chunked/start handler 'chunk-info/start)
      handler)))
