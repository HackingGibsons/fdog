(format t "Loading default configuration: ~A~%" *package*)

(using-configuration!
 (with-server ("control" :addr "localhost" :port 1337 :chroot "./")
   (with-host ("localhost")
     (make-route "/" (make-handler :send-spec "tcp://127.0.0.1:13375" ;; 5 => S => Send, Get it?
                                   :send-ident "control-send-ident"
                                   :recv-spec "tcp://127.0.0.1:13372")) ;; 2 => R => Recieve, right? ..right? :(
     (make-route "/static/" (make-dir "./public/")))))

(format t "Loaded!~%")
