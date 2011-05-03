(format t "Loading default configuration: ~A~%" *package*)

(using-configuration!
 (with-server ("control" :addr "localhost" :port 1337 :chroot "./")
   (with-host ("localhost")
     (make-route "/" (make-dir "./public/")))))

(format t "Loaded!~%")
