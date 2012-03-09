(defpackage #:forwarder-agent
  (:documentation "Fdog forwarder agent")
  (:use #:cl)
  (:use #:afdog
        #:agent
        #:log5
        #:alexandria
        #:json
        #:fdog-agent)
  (:import-from :arnesi
                :it
                :awhen
                :if-bind
                :when-bind)
  (:export :forwarder-agent
           :load-forwarder-json
           :*forwarder-server*
           :*forwarder-server-port*))

(in-package :forwarder-agent)

(defcategory forwarder-agent)

;; Knobs
(defvar *forwarder-server* "forwarder"
  "The name of the forwarder server")
(defvar *forwarder-server-port* 13374
  "Port the forwarder server runs on")
(defvar *forwarder-filename* "forwarders.json"
  "Filename to save forwarder state to")
(defvar *forwarder-filename-tmp* ".forwarders.json.tmp"
  "Temp file for writing state atomically")

;; Filename helpers
(defun forwarder-file-path (agent)
  (file-path agent *forwarder-filename*))
(defun forwarder-file-path-tmp (agent)
  (file-path agent *forwarder-filename-tmp*))

(defun file-path (agent filename)
  (merge-pathnames filename (merge-pathnames "server/" (agent-root agent))))

(defun handler-name (name route)
  (format nil "forwarder-~A-~A" name route))
