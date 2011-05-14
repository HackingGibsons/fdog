(defpackage :fdog-handler
  (:use :cl :fdog-models :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)
  (:export :request-handler
             ;;Slot Access
             :request-handler-sub
             :request-handler-pub
             :request-handler-processors
             :request-handler-interval
             ;; Methods
             :request-handler-m2-handler
             :request-handler-start
             :request-handler-stop
             :request-handler-running-p
             :request-handler-add-string-responder
             ;; Bridge builders
             :configure-bridges-for))
(in-package :fdog-handler)

