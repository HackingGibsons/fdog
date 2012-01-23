(in-package :afdog-tests)

;;; Fixtures needed: mongrel2-agent, forwarder-agent

(def-test (can-create-forwarder :group forwarder-agent-tests)
    (:seq (:eql :need-filled)
          (:eql :saw-forwarder)
          (:eql :server-exists)
          (:eql :handler-exists))
  ;;; Announce "need forwarder named foo"
  ;;; the handler gets created and announced
  ;;; check the handler exists
  (list
   (with-agent-conversation (m e) forwarder-agent-uuid
     (log-for (trace) "before need")
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "test" :hostpaths (("api.example.com" . "/")))
                            )))
     (log-for (trace) "after need")
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (or filled
                       (and (equalp (car msg) :filled) msg))))
          ((and filled
                (getf filled :forwarder))
           :need-filled)))
   (with-agent-conversation (m e) forwarder-agent-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (forwarders (getf (getf (getf msg :info) :provides) :forwarders)))
          ((and forwarders
                (find "test" (loop for i in hostpaths collect (car i)) :test #'string=))
           :saw-forwarder)))
   :server-exists
   :handler-exists))

(def-test (can-create-forwarder-with-multiple-hostpaths :group forwarder-agent-tests) (:eql :pending)
  :pending)

(def-test (forwarder-agent-remove :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder foo deleted"
  ;;; handler deleted, announced
  :pending)

(def-test (can-remove-multiple-forwarders :group forwarder-agent-tests) (:eql :pending))

(def-test (forwarder-agent-cull :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder culling, save foo and bar"
  ;;; Other handlers deleted
  ;;; check foo and bar exist, but not baz
  :pending)

(def-test (forwarder-agent-stores-forwarders :group forwarder-agent-tests) (:eql :pending)
  :pending)

(def-test (forwarder-agent-restores-forwarders-after-restart :group forwarder-agent-tests) (:eql :pending)
  :pending)
