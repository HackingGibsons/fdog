(in-package :agent)

(defclass agent-appendix (standard-beating-organ) nil
  (:documentation "An organ that exists solely to exist. Built to scaffold the messaging architecture")
  (:default-initargs . (:tag :appendix)))

