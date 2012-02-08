(in-package :afdog-tests)

(def-test (can-create-forwarder :group forwarder-agent-tests)
    (:seq (:eql :need-filled)
          (:eql :saw-forwarder)
          (:eql :server-exists)
          (:eql :handlers-exist))
  ;;; Announce "need forwarder named foo"
  ;;; the handler gets created and announced
  ;;; check the handler exists
  (list
   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "test" :hosts ("api.example.com" "api2.example.com") :routes (("root" . "/") ("one" . "/1/"))))) (msg)
       (awhen (getf msg :filled)
         (when (getf msg :forwarder)
              :need-filled)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (find "test" (loop for i in forwarders collect (car i)) :test #'string=)
            :saw-forwarder)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
       (when (find *forwarder-server* (loop for i in servers collect (car i)) :test #'string=)
            :server-exists)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
       (let* ((forwarder-server (assoc *forwarder-server* servers :test #'string=))
              (handler-root (assoc "forwarder-test-root" (cdr forwarder-server) :test #'string=))
              (handler-one (assoc "forwarder-test-one" (cdr forwarder-server) :test #'string=)))
         (when (and handler-root handler-one)
           :handlers-exist))))))

(def-test (forwarder-agent-remove :group forwarder-agent-tests
  :setup (progn
           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :forwarder
                                    :forwarder (:name "remove1" :hosts ("api2.example.com") :routes (("default" . "/r1/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :forwarder-1-added)))

           (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "remove2" :hosts ("api2.example.com") :routes (("default" . "/r2/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :forwarder-2-added)))

           (wait-for-agent-message (forwarder-agent-uuid) (msg)
             (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
               (when (and (find "remove1" (loop for i in forwarders collect (car i)) :test #'string=)
                          (find "remove2" (loop for i in forwarders collect (car i)) :test #'string=))
                 :forwarders-announced)))

           (wait-for-agent-message (mongrel2-uuid) (msg)
             (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
               (let* ((forwarder-server (assoc *forwarder-server* servers :test #'string=))
                      (handler1 (assoc "forwarder-remove1-default" (cdr forwarder-server) :test #'string=))
                      (handler2 (assoc "forwarder-remove2-default" (cdr forwarder-server) :test #'string=)))
                 (when (and handler1 handler2)
                   :handlers-announced))))))

    (:seq (:eql :forwarders-removed)
          (:eql :forwarders-gone)
          (:eql :handlers-removed))
  (list
   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :remove-forwarders
                            :remove-forwarders (:names ("remove1" "remove2")))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :remove-forwarders)
         :forwarders-removed)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind provides (getf (getf msg :info) :provides)
       (let ((forwarders (getf provides :forwarders)))
         (when (and (null (find "remove1" (loop for i in forwarders collect (car i)) :test #'string=))
                    (null (find "remove2" (loop for i in forwarders collect (car i)) :test #'string=)))
           :forwarders-gone))))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
       (let* ((forwarder-server (assoc *forwarder-server* servers :test #'string=))
              (handler1 (assoc "forwarder-remove1-default" (cdr forwarder-server) :test #'string=))
              (handler2 (assoc "forwarder-remove2-default" (cdr forwarder-server) :test #'string=)))
         (when (and (null handler1) (null handler2))
           :handlers-removed))))))

(def-test (forwarder-agent-keep :group forwarder-agent-tests
  :setup (progn
           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :forwarder
                                    :forwarder (:name "cull1" :hosts ("api2.example.com") :routes (("default" . "/c1/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :forwarder-1-added)))

           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :forwarder
                                    :forwarder (:name "cull2" :hosts ("api2.example.com") :routes (("default" . "/c2/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :forwarder-2-added)))

           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :forwarder
                                    :forwarder (:name "cull3" :hosts ("api2.example.com") :routes (("default" . "/c3/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :forwarder-3-added)))

           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :forwarder
                                    :forwarder (:name "cull4" :hosts ("api2.example.com") :routes (("default" . "/c4/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :forwarder-4-added)))

           (wait-for-agent-message (forwarder-agent-uuid) (msg)
             (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
               (when (and (find "cull1" (loop for i in forwarders collect (car i)) :test #'string=)
                          (find "cull2" (loop for i in forwarders collect (car i)) :test #'string=)
                          (find "cull3" (loop for i in forwarders collect (car i)) :test #'string=)
                          (find "cull4" (loop for i in forwarders collect (car i)) :test #'string=))
                 :forwarders-announced)))

           (wait-for-agent-message (mongrel2-uuid) (msg)
             (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
               (let* ((forwarder-server (assoc *forwarder-server* servers :test #'string=))
                      (handler1 (assoc "forwarder-cull1-default" (cdr forwarder-server) :test #'string=))
                      (handler2 (assoc "forwarder-cull2-default" (cdr forwarder-server) :test #'string=))
                      (handler3 (assoc "forwarder-cull3-default" (cdr forwarder-server) :test #'string=))
                      (handler4 (assoc "forwarder-cull4-default" (cdr forwarder-server) :test #'string=)))
                 (when (and handler1 handler2 handler3 handler4)
                   :handlers-announced))))))

    (:seq (:eql :forwarders-culled)
          (:eql :saved-forwarders-kept-and-other-forwarders-gone)
          (:eql :saved-handlers-kept-and-other-handlers-gone))
  (list
   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :keep-forwarders
                            :keep-forwarders (:names ("cull1" "cull2")))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :keep-forwarders)
         :forwarders-culled)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (and (find "cull1" (loop for i in forwarders collect (car i)) :test #'string=)
                  (find "cull2" (loop for i in forwarders collect (car i)) :test #'string=)
                  (null (find "cull3" (loop for i in forwarders collect (car i)) :test #'string=))
                  (null (find "cull4" (loop for i in forwarders collect (car i)) :test #'string=)))
         :saved-forwarders-kept-and-other-forwarders-gone)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (when-bind servers (getf (getf (getf msg :info) :provides) :servers)
       (let* ((forwarder-server (assoc *forwarder-server* servers :test #'string=))
              (handler1 (assoc "forwarder-cull1-default" (cdr forwarder-server) :test #'string=))
              (handler2 (assoc "forwarder-cull2-default" (cdr forwarder-server) :test #'string=))
              (handler3 (assoc "forwarder-cull3-default" (cdr forwarder-server) :test #'string=))
              (handler4 (assoc "forwarder-cull4-default" (cdr forwarder-server) :test #'string=)))
         (when (and handler1 handler2 (null handler3) (null handler4))
           :saved-handlers-kept-and-other-handlers-gone))))))

(def-test (forwarder-agent-stores-forwarders :group forwarder-agent-tests
  :setup (progn
           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :keep-forwarders
                                    :keep-forwarders (:names nil))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :keep-forwarders)
                 :all-forwarders-culled)))

           (wait-for-agent-message (forwarder-agent-uuid) (msg)
             (let ((forwarders (getf (getf (getf msg :info) :provides) :forwarders)))
               (unless forwarders
                 :announcing-zero-forwarders)))))

    (:seq (:eql :file-empty)
          (:eql :forwarder-added)
          (:eql :forwarder-announced)
          (:eql :forwarder-exists-in-file)
          (:eql :forwarder-removed)
          (:eql :forwarder-gone)
          (:eql :file-empty))
  (list
   (let ((file (merge-pathnames *forwarder-filename* (merge-pathnames "server/" *root*))))
     (with-open-file (in file)
       (unless (forwarder-agent:load-forwarder-json in)
         :file-empty)))

   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "saveme" :hosts ("api2.example.com") :routes (("default" . "/s/"))))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :forwarder)
         :forwarder-added)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (find "saveme" (loop for i in forwarders collect (car i)) :test #'string=)
         :forwarder-announced)))

   (let ((file (merge-pathnames *forwarder-filename* (merge-pathnames "server/" *root*))))
     (with-open-file (in file)
       (when-bind forwarders (forwarder-agent:load-forwarder-json in)
         (when (find "saveme" (loop for i in forwarders collect (getf i :name)) :test #'string=)
           :forwarder-exists-in-file))))


   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :remove-forwarders
                            :remove-forwarders (:names ("saveme")))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :remove-forwarders)
         :forwarder-removed)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (let ((forwarders (getf (getf (getf msg :info) :provides) :forwarders)))
       (when (null (find "saveme" (loop for i in forwarders collect (car i)) :test #'string=))
         :forwarder-gone)))

   (let ((file (merge-pathnames *forwarder-filename* (merge-pathnames "server/" *root*))))
     (with-open-file (in file)
       (unless (forwarder-agent:load-forwarder-json in)
         :file-empty)))))

(def-test (forwarder-agent-restores-forwarders-after-restart :group forwarder-agent-tests
  :setup (progn
           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :keep-forwarders
                                    :keep-forwarders (:names nil))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :keep-forwarders)
                 :all-forwarders-culled)))

           (let ((file (merge-pathnames *forwarder-filename* (merge-pathnames "server/" *root*))))
             (with-open-file (in file :if-does-not-exist nil)
               (unless (and in (forwarder-agent:load-forwarder-json in))
                 :file-empty)))

           (wait-for-agent-message (forwarder-agent-uuid :request
                           `(:agent :need
                                    :need :forwarder
                                    :forwarder (:name "restore" :hosts ("api2-example.com") :routes (("default" . "/rs/"))))) (msg)
             (awhen (getf msg :filled)
               (when (getf msg :forwarder)
                 :need-filled)))))
    
    (:seq (:eql :agent-killed)
          (:eql :agent-dead)
          (:eql :handler-requested)
          (:eql :forwarder-announced))
  (list

   ;; send kill request to the hypervisor, listen for forwarder agent
   ;; announce
   (progn
     (send-message-blindly forwarder-agent-uuid :request
                   `(:agent :kill :kill ,forwarder-agent-uuid))
     :agent-killed)

   (wait-for-agent-message (hypervisor-uuid) (msg)
     (when-bind peers (getf (getf msg :info) :peers)
       (unless (assoc forwarder-agent-uuid peers :test #'string=)
         :agent-dead)))

   ;; ZMQ can wait on a socket that isn't open on the other side
   ;; So when the agent restarts we receive messages immediately
   (wait-for-agent-message (forwarder-agent-uuid :timeout 30) (msg)
     (awhen (and (getf msg :need)
                 (getf msg :handler))
       (when (string= "forwarder-restore-default" (getf it :name))
         :handler-requested)))

   (wait-for-agent-message (forwarder-agent-uuid) (msg)
     (when-bind forwarders (getf (getf (getf msg :info) :provides) :forwarders)
       (when (find "restore" (loop for i in forwarders collect (car i)) :test #'string=)
         :forwarder-announced)))))

(def-test (missing-handlers-restored :group forwarder-agent-tests
  :setup (progn
   (wait-for-agent-message (forwarder-agent-uuid :request
                   `(:agent :need
                            :need :forwarder
                            :forwarder (:name "missing" :hosts ("api2.example.com") :routes (("default" . "/m/"))))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :forwarder)
         :forwarder-added)))))

    (:seq (:eql :handler-removed)
          (:eql :handler-restored))
  (list
   (wait-for-agent-message (mongrel2-uuid :request
                   `(:agent :need
                            :need :remove-handlers
                            :remove-handlers (:server ,*forwarder-server* :names ("forwarder-missing-default")))) (msg)
     (awhen (getf msg :filled)
       (when (getf msg :remove-handlers)
         :handler-removed)))

   (wait-for-agent-message (mongrel2-uuid) (msg)
     (awhen (getf msg :handler)
       (when (string= "forwarder-missing-default" (getf it :name))
         :handler-restored)))))
