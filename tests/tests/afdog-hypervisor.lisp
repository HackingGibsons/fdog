(in-package :afdog-tests)

(def-test (afh-can-spawn-mongrel2-agent :group afdog-hypervisor-agent-tests :fixtures (afdog-hypervisor-agent-fixture kill-everything-fixture db-path-fixture))
    (:eql :mongrel2-running)
  (with-agent-conversation (m e  :timeout 60) afdog-hypervisor-uuid
    (do* ((connected (awhen (make-hash-table :test 'equalp)
                       (setf (gethash afdog-hypervisor-uuid it) :connected) 
                       it) 
                     connected)
          (msg (parse-message (read-message m)) 
              (parse-message  (read-message m)))
          (info (getf msg :info) (getf msg :info))
          (peers (getf msg :peers) (getf msg :peers)))          
         (nil :mongrel2-running) 
      (format t "Msg ~S~%" msg)
      (mapc #'(lambda (peer)
                (destructuring-bind (uuid &rest _) peer
                  (unless (gethash uuid connected)
                    (format t "Connecting to ~A~%" uuid)
                    (zmq:connect m (local-ipc-addr uuid :mouth))
                    (setf (gethash uuid connected) :connected))))
            peers))))
