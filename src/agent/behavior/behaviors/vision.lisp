(in-package :agent)

(defbehavior look-when-told (:on (:command :look :from :head) :do :invoke-with-event) (organ event)
  (log-for (trace organ) "organ: ~A event: ~A look event" organ event)
  (let* ((thing (getf event :at)))
    (log-for (trace organ) "calling see on ~A with subject: ~A and info: ~A" organ (first thing) (rest thing))
    (apply #'see organ (first thing) (rest thing))))
