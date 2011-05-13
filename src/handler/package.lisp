(defpackage :fdog-handler
  (:use :cl :fdog-models :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)
  (:export :request-handler
             ;;Slot Access
             :request-handler-sub
             :request-handler-pub
             :request-handler-processor
             :request-handler-interval
             ;; Methods
             :request-handler-m2-handler
             :request-handler-start
             :request-handler-stop
             :request-handler-running-p))
(in-package :fdog-handler)

