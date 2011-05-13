(defpackage :fdog-control
  (:use :cl :fdog-models :bordeaux-threads :fdog-handler)
  (:shadowing-import-from :log5 :log-for)
  (:export :run))
(in-package :fdog-control)

(defparameter *ident* "control-ident")
(defparameter *m2-send* "tcp://127.0.0.1:13375")
(defparameter *m2-recv* "tcp://127.0.0.1:13372")

;;; Scaffold
(defun response ()
  (format nil "~A:~A" (get-universal-time) (current-thread)))

(defun req-fun (req-handler request raw)
  ;; (log-for (dribble) "Raw request: ~A" (flex:octets-to-string raw))
  ;; (log-for (dribble) "Cooked request: ~A" (or (and request
  ;;                                                  (list
  ;;                                                   :headers (m2cl:request-headers request)
  ;;                                                   :body (m2cl:request-body request)
  ;;                                                   :data (m2cl:request-data request)))
  ;;                                             "Is nil"))
  (let ((m2-handler (request-handler-responder-handler req-handler)))
    (unless (m2cl::request-disconnect? request)
      (m2cl:handler-send-http m2-handler (response) :request request)
      (m2cl:handler-close m2-handler :request request))))



(defun run (&rest args &key &allow-other-keys)
  (declare (ignorable args))
  (let ((handler (make-instance 'request-handler :ident *ident* :proc 'req-fun
                                :sub *m2-send* :pub *m2-recv*)))
    (log-for (dribble) "Starting request responder")
    (request-handler-start handler)
    handler))
