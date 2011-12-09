(in-package :mongrel2-agent)

(defmethod link-server ((organ agent::standard-organ) (server fdog-models:mongrel2-server) config)
  (flet ((make-mongrel2-arguments (server config)
           (let ((uuid (fdog-models:mongrel2-server-uuid server)))
             `(:path "/usr/bin/env" :args ("mongrel2" ,(namestring config) ,uuid)))))

    (let ((arguments (make-mongrel2-arguments server config))
          (pid (fdog-models:mongrel2-server-pid server)))

      (log-for (mongrel2-agent trace) "Found mongrel2 server ~A to link with make arguments of ~A." server arguments)

      (send-message organ :command `(:command :link
                                              :link :process
                                              :process (:pid ,pid ,@arguments))))))
