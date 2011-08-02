(in-package :fdog-tests)

(defun http->string (url &rest args)
  "A slightly repackaged veresion of drakma's client.
Mostly exists to close the stream when required and repackage the 7-value
return as something more edible (plist). Though discarding some values that
are less usefull to string construction."
  (multiple-value-bind
        (response status-code headers uri stream must-close reason-phrase)
        (apply 'drakma:http-request `(,url ,@args))

    (when must-close (close stream))

    (values response
            (list :status-code status-code
                  :headers     headers
                  :uri         uri
                  :reason      reason-phrase))))
