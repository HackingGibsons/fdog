(defpackage #:fdog
  (:use :mongrel2-agent)
  (:use :afdog-hypervisor-agent)
  (:export :mongrel2-agent
           :afdog-hypervisor-agent))

(in-package :fdog)
