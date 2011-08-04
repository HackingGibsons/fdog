(in-package :fdog-tests)

(defun http->string (url &key (timeout 3) (method :GET) content-type content)
  "A slightly repackaged veresion of drakma's client.
Mostly exists to close the stream when required and repackage the 7-value
return as something more edible (plist). Though discarding some values that
are less usefull to string construction.
The first value returned is only a string if drakma thinks it is, otherwise
it will be an octet vector"
  (bt:with-timeout (timeout)
    (handler-case
        (multiple-value-bind
              (response status-code headers uri stream must-close reason-phrase)
              (let ((args `(:method ,method :content-type ,content-type :content ,content)))
                (apply 'drakma:http-request `(,url ,@args)))

          (when must-close (close stream))

          (values response
                  (list :status-code status-code
                        :headers     headers
                        :uri         uri
                        :reason      reason-phrase)))

      (bt:timeout () nil))))

(defun http->json (url &rest keys)
  "Wrap `http->string' to parse out JSON, and act more leniently in the face
of octet vectors."
  (multiple-value-bind (res meta) (apply #'http->string `(,url :content-type "application/json" ,@keys))
    (values (typecase res
              (string (json:decode-json-from-string res))
              (vector (json:decode-json-from-string (flex:octets-to-string res)))
              (otherwise res))
            meta)))

(defun make (target &key (dir (asdf:system-source-directory :fdog)))
  "Wrapper for executing gnumake on a target. Defaults to running in the `:fdog' package home,
tweakable with `:dir' keyword"
  (external-program:run "make" `("-C" ,(namestring dir) ,target)
                        :output t :error t))
