;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ___                         ___           ___               ;;;
;;;             /\__\         _____         /\  \         /\__\              ;;;
;;;            /:/ _/_       /::\  \       /::\  \       /:/ _/_             ;;;
;;;           /:/ /\__\     /:/\:\  \     /:/\:\  \     /:/ /\  \            ;;;
;;;          /:/ /:/  /    /:/  \:\__\   /:/  \:\  \   /:/ /::\  \           ;;;
;;;         /:/_/:/  /    /:/__/ \:|__| /:/__/ \:\__\ /:/__\/\:\__\          ;;;
;;;         \:\/:/  /     \:\  \ /:/  / \:\  \ /:/  / \:\  \ /:/  /          ;;;
;;;          \::/__/       \:\  /:/  /   \:\  /:/  /   \:\  /:/  /           ;;;
;;;           \:\  \        \:\/:/  /     \:\/:/  /     \:\/:/  /            ;;;
;;;            \:\__\        \::/  /       \::/  /       \::/  /             ;;;
;;;             \/__/         \/__/         \/__/         \/__/              ;;;
;;;                                                                          ;;;
;;;                          [ A Bluespace Router ]                          ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This readme serves as the bootstrap
;;;
;;; Requirements:
;;;   * SBCL 1.0.47
;;;   * Quicklisp
;;;   * Fdog path in the ASDF registery
;;;
;;; I use the following ASDF path loader in .sbclrc:
;;
#|
(let ((registry-root #p"~/src/tools/ASDF/*"))
  (loop for path in (directory registry-root)
        for dirpath = (make-pathname :directory (pathname-directory path))
     ;; Only act on paths that are also directories in the root
     if (equal path dirpath) do
       (pushnew path asdf:*central-registry* :test #'equal)))
|#
;;
;;;
;;; For now, I'm using the following bootstrap to prep the image:

;; This should point to the checkout
(setf *default-pathname-defaults* (truename #P"~/src/work/fdog/"))

;; Load me!
(ql:quickload :fdog)

;; Init (Db connection, etc)
(fdog:init)

;; This outputs querires as they are run, for debugging
(clsql:start-sql-recording)

