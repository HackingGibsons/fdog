(in-package :agent-host)

(defun s2us (s)
  "Seconds->uSeconds"
  (round (* s 1000000)))

(defun sock-id (sock &optional (inout :in))
  "Return the pointer address of the sock so we can use it in HTs under `equalp'"
  (format nil "~A-~S" (cffi:pointer-address sock) inout))

