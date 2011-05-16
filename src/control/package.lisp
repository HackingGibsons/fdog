(defpackage :fdog-control
  (:use :sb-mop)
  (:use :cl :fdog-models :bordeaux-threads :fdog-handler)
  (:shadowing-import-from :log5 :log-for)
  (:export :*control-interface*
           :init-control-interface
           ;; Interface API
           ;;; Data access
           :fdog-interface
           :fdog-interface-server
           :fdog-interface-routes
           ;;; General
           :interface-stop
           ;;; Server control
           :interface-start-server
           :interface-stop-server
           ;;; Bridges control
           :interface-start-bridges
           :interface-stop-bridges
           ;;; Query and configuration
           :interface-bridge-matching
           :interface-configure-bridges))
