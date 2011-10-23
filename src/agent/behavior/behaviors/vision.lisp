(in-package :agent)

(defbehavior look-when-told (:on (:command :look :from :head) :do :invoke-with-event) (organ event)
  (log-for (trace organ) "organ: ~A event: ~A look event" organ event)
  (let* ((thing (getf event :at)))
    (log-for (trace organ) "calling see on ~A with subject: ~A and info: ~A" organ (first thing) (rest thing))
    (apply #'see organ (first thing) thing)))

(defclass watch-mixin ()
  ())

(defbehavior watch-when-told (:or ((:on (:command :watch :from :head))
                                   (:on (:command :stop-watching :from :head)))
                                  :include (watch-mixin)
                                  :do :invoke-with-event) (organ event)
  (log-for (trace organ) "organ: ~A event: ~A watch event" organ event)
  ; (:HEAD :COMMAND :COMMAND :WATCH :WATCH (:PROCESS :PID :PID PID))
  (case (getf event :command)
    (:watch (watch behavior (getf event :watch)))
    (:stop-watching (stop-watching behavior (getf event :stop-watching)))))

(defmethod watch ((behavior watch-mixin) subject)
  :TODO)

(defmethod stop-watching ((behavior watch-mixin) subject)
  :TODO)