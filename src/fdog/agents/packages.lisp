(defpackage #:mongrel2-agent
  (:documentation "Mongrel2 afdog agent and compontents.")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5)
  (:export :mongrel2-agent
           :rooted-agent-mixin
           :ensure-mongrel2-root-layout
           :initialize-mongrel2-configuration))
