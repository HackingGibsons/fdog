(defpackage :fdog-control
  (:use :cl :fdog-models :bordeaux-threads :fdog-handler)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defparameter *ident* "control-ident")
(defparameter *m2-send* "tcp://127.0.0.1:13375")
(defparameter *m2-recv* "tcp://127.0.0.1:13372")

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
    '((:X-hello-world . "I am awesome")))

  (defparameter *chunked-handler*
    (let ((handler (configure-bridges-for *handler*)))
      (request-handler-add-chunked/stop handler)

      (request-handler-add-chunked/start handler 'chunk-info/start)
      handler)))
