(format t "Loading default configuration: ~A~%" *package*)

(using-configuration!
 (with-server ("control" :addr "localhost" :port 1337 :chroot "./")
   (with-host ("localhost")
     (make-route "/" (make-dir "public/"))))

 (with-server ("routing" :addr "localhost" :port 31337 :chroot "./")
   (with-host ("superlocalhost")
     (make-route "/handler/" (make-handler :send-spec "tcp://127.0.0.1:9999"
                                           :send-ident "54c6755b-9658-40f4-9c2a-fe81a816345e"
                                           :recv-spec "tcp://127.0.0.1:9998"))
     (make-route "/proxy/" (make-proxy "localhost" 8090)))))
