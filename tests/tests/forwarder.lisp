(in-package :afdog-tests)

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
     (zmq:send! e (prepare-message
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "test" :hostpaths (("api2.example.com" . "/"))))))
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (filled (and (equalp (car msg) :filled) msg)
                   (and (equalp (car msg) :filled) msg)))
          ((and filled
                (getf filled :forwarder))
           :need-filled)))

   (with-agent-conversation (m e) forwarder-agent-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (forwarders (getf (getf (getf msg :info) :provides) :forwarders)
                       (getf (getf (getf msg :info) :provides) :forwarders)))
          ((and forwarders
                (find "test" (loop for i in forwarders collect (car i)) :test #'string=))
           :saw-forwarder)))

   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (servers (getf (getf (getf msg :info) :provides) :servers)
                    (getf (getf (getf msg :info) :provides) :servers)))
          ((and servers
                (find "forwarder" (loop for i in servers collect (car i)) :test #'string=))
           :server-exists)))

   (with-agent-conversation (m e) mongrel2-uuid
     (do* ((msg (parse-message (read-message m))
                (parse-message (read-message m)))
           (servers (getf (getf (getf msg :info) :provides) :servers)
                    (getf (getf (getf msg :info) :provides) :servers))
           (forwarder-server (assoc "forwarder" servers :test #'string=)
                             (assoc "forwarder" servers :test #'string=))
           (handler (assoc "forwarder-test" (cdr forwarder-server) :test #'string=)
                    (assoc "forwarder-test" (cdr forwarder-server) :test #'string=)))
          (handler
               :handler-exists)))))

(def-test (can-create-forwarder-with-multiple-hostpaths :group forwarder-agent-tests) (:eql :pending)
  :pending)

(def-test (forwarder-agent-remove :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder foo deleted"
  ;;; handler deleted, announced
  :pending)

(def-test (can-remove-multiple-forwarders :group forwarder-agent-tests) (:eql :pending)
  :pending)

(def-test (forwarder-agent-cull :group forwarder-agent-tests) (:eql :pending)
  ;;; Announce "need forwarder culling, save foo and bar"
  ;;; Other handlers deleted
  ;;; check foo and bar exist, but not baz
  :pending)

(def-test (forwarder-agent-stores-forwarders :group forwarder-agent-tests) (:eql :pending)
  ;;; Add a forwarder
  ;;; Forwarder gets stored in the file
  ;;; Remove the forwarder
  ;;; It's removed from the file
  :pending)

(def-test (forwarder-agent-restores-forwarders-after-restart :group forwarder-agent-tests) (:eql :pending)
  ;;; Add a forwarder
  ;;; Restart the agent
  ;;; That forwarder exists
  :pending)
