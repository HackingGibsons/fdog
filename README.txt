;; -*- mode: Lisp;  -*-
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
;;;   * Mongrel2 1.6                                                         ;;;
;;;   * 0MQ 2.1.4                                                            ;;;
;;;   * SBCL 1.0.48                                                          ;;;
;;;   * patched CLSQL from https://github.com/nikodemus/clsql.git            ;;;
;;;   * M2CL (forked) from https://github.com/sshirokov/m2cl.git             ;;;
;;;   * Quicklisp                                                            ;;;
;;;   * Fdog path in the ASDF registry (see: user-tools/asdf-loader.lisp)    ;;;
;;;                                                                          ;;;
;;; Note:                                                                    ;;;
;;;   * This readme is somewhat stale, see the Makefile                      ;;;
;;;                                                                          ;;;
;;; Testing:                                                                 ;;;
;;;   * The test suite can be run with (asdf:test-system :fdog)              ;;;
;;;     It should run the unit and integration tests, some of which          ;;;
;;;     will spin up a Mongrel2 and send requests from the code              ;;;
;;;     so I'm not kidding at all in the requirements. ;)                    ;;;
;;;   * Coverage Reporting                                                   ;;;
;;;     $ bin/coverage                                                       ;;;
;;;     [ This will churn for a while ]                                      ;;;
;;;     $ open coverage/cover-index.html                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;[ Development Bootstrap ];;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knobs
(defparameter *echo-queries* t)

;; Load and init
(ql:quickload :fdog)
(fdog:init)

;; Some developer noise that considers the above knobs
(when *echo-queries*
  (clsql:start-sql-recording))

;; Start
(fdog:start)
