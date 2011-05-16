(defpackage :fdog-handler
  (:use :cl :fdog-models :bordeaux-threads)
  (:shadowing-import-from :log5 :log-for)
  (:export :request-handler
             ;;Slot Access
             :handler-bridge-db-handler
             :request-handler-sub
             :request-handler-pub
             :request-handler-processors
             :request-handler-interval
             ;; Methods
             :request-handler-m2-handler
             :request-handler-start
             :request-handler-stop
             :request-handler-running-p
             ;; Responder chain builders
             :request-handler-add-responder
               :make-request-handler-string-responder
                 :request-handler-add-string-responder
               :request-handler-make-chunked-responder/start
               :request-handler-make-chunked-responder/chunk
               :request-handler-make-chunked-responder/stop
               :request-handler-make-chunked-responder/trailer
                 :request-handler-add-chunked/start
                 :request-handler-add-chunked/chunk
                 :request-handler-add-chunked/stop
                 :request-handler-add-chunked/trailer
             ;; Bridge builders
             :configure-bridges-for))
(in-package :fdog-handler)

