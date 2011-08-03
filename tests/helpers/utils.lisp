(in-package :fdog-tests)

(defun http->string (url &rest args)
  "A slightly repackaged veresion of drakma's client.
Mostly exists to close the stream when required and repackage the 7-value
return as something more edible (plist). Though discarding some values that
are less usefull to string construction."
  (bt:with-timeout (3)
    (handler-case
        (multiple-value-bind
              (response status-code headers uri stream must-close reason-phrase)
              (apply 'drakma:http-request `(,url ,@args))

          (when must-close (close stream))

          (values response
                  (list :status-code status-code
                        :headers     headers
                        :uri         uri
                        :reason      reason-phrase)))

      (bt:timeout () nil))))

(defun make (target &key (dir (asdf:system-source-directory :fdog)))
  "Wrapper for executing gnumake on a target. Defaults to running in the `:fdog' package home,
tweakable with `:dir' keyword"
  (external-program:run "make" `("-C" ,(namestring dir) ,target)
                        :output t :error t))
