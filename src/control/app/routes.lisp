(in-package :fdog-control)

(defun root/router (handler request raw)
  (with-dispatch-on (m2cl:request-path request) &route
    (funcall &route handler request raw)

    (:exact "/" :responder 'root/respond)
    (:regex "^/section/[\\w_-]+/?" :responder 'root/section)

    (:404 :responder 'root/404)))
