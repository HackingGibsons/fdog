(defpackage :fdog-control
  (:use :sb-mop)
  (:use :cl :fdog-models :bordeaux-threads :fdog-handler)
  (:shadowing-import-from :log5 :log-for)
  (:export :*control-interface*
           :fdog-interface
           :fdog-interface-server
           :fdog-interface-routes
           :interface-bridge-matching))
(in-package :fdog-control)

(defparameter *control-interface* nil
  "The interface for interacting with FDOG externally")

(defmethod init-control-interface (&key (server "control"))
  "Creates a new control interface, killing the old, and starts it."
  (when *control-interface*
    (interface-stop *control-interface*))
  (let ((interface (setf *control-interface* (make-instance 'fdog-interface :server server))))
    (interface-start-server interface))

  *control-interface*)

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
