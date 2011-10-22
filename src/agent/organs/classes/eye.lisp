(in-package :agent)

(defclass agent-eye (standard-beating-organ)
  ()
  (:documentation "`standard-agent' peepers")
  (:default-initargs . (:tag :eye)))

(defbehavior look-when-told (:on (:command :look :from :head) :do :invoke-with-event) (organ event)
  (log-for (trace) "organ: ~A event: ~A look event~%" organ event))

(defmethod initialize-instance :after ((eye agent-eye) &key)
  (make-look-when-told eye))