(in-package :afdog-tests)

;; Accounts tests
;; No API key - expect 401
;; Incorrect API key - expect 401
;; Correct API key - expect "correct" behavior (whatever that is)

;; later - test that a correct API key exists in redis and a timeout
;; exists
