(in-package :afdog)

(log5:defoutput human-time (multiple-value-bind (second minute hour date month year)
                               (decode-universal-time (get-universal-time))
                             (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))


(defun start-logging (&key (default t))
  (when default
    (log5:start-sender 'default
                       (log5:stream-sender :location *error-output*)
;                       :category-spec '(log5:dribble+)
                       :category-spec '(log5:warn+)
                       :output-spec '(human-time log5:category log5:message))))

(defun stop-logging ()
  (log5:stop-all-senders))
