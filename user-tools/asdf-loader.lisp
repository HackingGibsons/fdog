;; This is most useful somewhere like ~/.sbclrc not in a project
;; walks the directory named `registry-root' adding all subdirs to the ASDF central registry
;; I use this to satisfy "Fdog in the ASDF registry" requrement from the README
(let ((registry-root #p"~/src/tools/ASDF/*"))
  (loop for path in (directory registry-root)
        for dirpath = (make-pathname :directory (pathname-directory path))
     ;; Only act on paths that are also directories in the root
     if (equal path dirpath) do
       (pushnew path asdf:*central-registry* :test #'equal)))
