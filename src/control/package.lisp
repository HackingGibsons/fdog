(defpackage :fdog-control
  (:use :cl :fdog-models :bordeaux-threads :fdog-handler)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defvar *interface-bridges* ()
  "Bridges configured as part of the web UI")

(defun init-web-ui (&key (name "control") uuid)
  (let ((server (car (fdog-m2sh:servers :uuid uuid :name name :refresh t))))
    (unless server (error "Can't find a server!"))
    (unless (mongrel2-server-running-p server)
      (if (eq (mongrel2-server-signal/block server :start) :timeout)
          (error "Timeout starting Mongrel2")))

    (mapcar #'request-handler-stop *interface-bridges*)
    (setf *interface-bridges* ())

    (dolist (route (mongrel2-host-routes (mongrel2-server-default-host server)) *interface-bridges*)
      (with-accessors ((target mongrel2-route-target)) route
        (typecase target
          (mongrel2-handler
           (log-for (trace) "Found handler: ~A" target)
           (pushnew (configure-bridges-for target) *interface-bridges*))
          (otherwise
           (if target
               (log-for (trace) "Let it be known there exists: ~A" target)
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
