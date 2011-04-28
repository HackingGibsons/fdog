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
;;;                                                                          ;;;
;;; This README serves as the bootstrap, eval it after you read it!          ;;;
;;;                                                                          ;;;
;;; Requirements:                                                            ;;;
;;;   * SBCL 1.0.47                                                          ;;;
;;;   * patched CLSQL from https://github.com/nikodemus/clsql.git            ;;;
;;;   * Quicklisp                                                            ;;;
;;;   * Fdog path in the ASDF registry (see: user-tools/asdf-loader.lisp)    ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;[ Development Bootstrap ];;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knobs
(defparameter *checkout-root* #P"~/src/work/fdog/")
(defparameter *echo-queries* t)

;; Sanity
(setf *default-pathname-defaults* (truename *checkout-root*))

;; Load and init
(ql:quickload :fdog)
(fdog:init)

;; Some developer noise that considers the above knobs
(when *echo-queries*
  (clsql:start-sql-recording))

