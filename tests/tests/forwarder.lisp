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
   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "test" :hostpaths (("api2.example.com" . "/"))))) (msg)
       (awhen (getf msg :filled)
         (when (getf msg :forwarder)
              :need-filled)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (find "test" (loop for i in forwarders collect (car i)) :test #'string=)
            :saw-forwarder)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
       (when (find "forwarder" (loop for i in servers collect (car i)) :test #'string=)
            :server-exists)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (let* ((servers (getf (getf (getf msg :info) :provides) :servers))
            (forwarder-server (assoc "forwarder" servers :test #'string=))
            (handler (assoc "forwarder-test" (cdr forwarder-server) :test #'string=)))
       (when handler
         :handler-exists)))))


(def-test (can-create-forwarder-with-multiple-hostpaths :group forwarder-agent-tests) (:eql :pending)
  :pending)

(def-test (forwarder-agent-remove :group forwarder-agent-tests)
    (:seq (:eql :forwarder-1-added)
          (:eql :forwarder-2-added)
          (:eql :forwarders-announced)
          (:eql :handlers-announced)
          (:eql :forwarders-removed)
          (:eql :forwarders-gone)
          (:eql :handlers-removed))
  (list
   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "remove1" :hostpaths (("api2.example.com" . "/r1/"))))) (msg)
       (awhen (getf msg :filled)
         (when (getf msg :forwarder)
           :forwarder-1-added)))

   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "remove2" :hostpaths (("api2.example.com" . "/r2/"))))) (msg)
       (awhen (getf msg :filled)
         (when (getf msg :forwarder)
           :forwarder-2-added)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (and (find "remove1" (loop for i in forwarders collect (car i)) :test #'string=)
                  (find "remove2" (loop for i in forwarders collect (car i)) :test #'string=))
         :forwarders-announced)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (let* ((servers (getf (getf (getf msg :info) :provides) :servers))
            (forwarder-server (assoc "forwarder" servers :test #'string=))
            (handler1 (assoc "forwarder-remove1" (cdr forwarder-server) :test #'string=))
            (handler2 (assoc "forwarder-remove2" (cdr forwarder-server) :test #'string=)))
       (when (and handler1 handler2)
         :handlers-announced)))

   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :remove-forwarders
                            :remove-forwarders (:names ("remove1" "remove2")))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :remove-forwarders)
         :forwarders-removed)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (and (null (find "remove1" (loop for i in forwarders collect (car i)) :test #'string=))
                  (null (find "remove2" (loop for i in forwarders collect (car i)) :test #'string=)))
         :forwarders-gone)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (let* ((servers (getf (getf (getf msg :info) :provides) :servers))
            (forwarder-server (assoc "forwarder" servers :test #'string=))
            (handler1 (assoc "forwarder-remove1" (cdr forwarder-server) :test #'string=))
            (handler2 (assoc "forwarder-remove2" (cdr forwarder-server) :test #'string=)))
       (when (and (null handler1) (null handler2))
         :handlers-removed)))))

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
