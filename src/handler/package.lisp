(defpackage :fdog-handler
  (:use :cl
        :sb-gray
        :bordeaux-threads
        :fdog-models :fdog-utils)
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
             :request-handler-respond-with-chain
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
                 :with-chunked-reply-chain
                 :with-chunked-reply-chain-response
                 :with-chunked-stream-reply
             ;; Bridge builders
             :configure-bridges-for
             ;; Utils
             :with-dispatch-on))
(in-package :fdog-handler)

