(in-package :fdog-cli)

(defun install-default-configuration ()
  (using-configuration!
   (with-server ("control" :bind "0.0.0.0" :port 1337 :chroot "./")
     (with-host ("localhost")
       (make-route "/" (make-handler :send-spec "tcp://127.0.0.1:1335" ;; 5 => S => Send, Get it?
                                     :send-ident "control-ident"
                                     :recv-spec "tcp://127.0.0.1:1332")) ;; 2 => R => Recieve, right? ..right? :(
       (make-route "/static/" (make-dir "./public/")))))

  (fdog-models:make-mongrel2-setting :certdir "certs/")
  (fdog-models:make-mongrel2-setting :superpoll.max_fd "65536")
  (fdog-models:make-mongrel2-setting :limits.tick_timer "1")
  (fdog-models:make-mongrel2-setting :limits.min_ping "60")
  (fdog-models:make-mongrel2-setting :limits.fdtask_stack "204800")
  (fdog-models:make-mongrel2-setting :limits.handler_stack "204800")
  (fdog-models:make-mongrel2-setting :limits.connection_stack_size "65536")
  (fdog-models:make-mongrel2-setting :superpoll.hot_dividend "2")
  (fdog-models:make-mongrel2-setting :limits.content_length "204800"))
