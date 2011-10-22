(in-package :agent)

(defclass agent-eye (standard-beating-organ)
  ()
  (:documentation "`standard-agent' peepers")
  (:default-initargs . (:tag :eye)))

(defcategory eye)
