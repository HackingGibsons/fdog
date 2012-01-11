(in-package :request-processing-agent)

(defcategory peer-collection)
(defmethod heard-message ((agent standard-agent) (organ agent-requesticle) (from (eql :agent)) (type (eql :info)) &rest info)
  "The requesticle hears an info message. It should examine it to see if contains
any handlers we're interested in and we should connect to them."
  (let* ((info (getf info :info))
         (provides (getf info :provides))
         (servers (getf provides :servers)))
    (labels ((try-connecting (name &key send recv &allow-other-keys)
               "Connect to the given handler if the name matches."
               (log-for (trace requesticle peer-collection) "Maybe connect to: ~S (~A ~A)" name send recv))

             (search-server (server-handlers)
               "Search a server description for handlers"
               (log-for (trace requesticle peer-collection) "Destructuring: ~S" server-handlers)
               (destructuring-bind (server &rest handlers) server-handlers
                 (declare (ignorable server))
                 (log-for (trace requesticle peer-collection) "Handlers: ~S" handlers)
                 (mapc #'(lambda (handler) (apply #'try-connecting handler)) handlers))))

      (log-for (trace requesticle peer-collection) "Requesticle hears info: ~S" info)
      (log-for (trace requesticle peer-collection) "Found servers: ~S" servers)
      (mapc #'search-server servers))))
