#lang racket
(require test-engine/racket-tests)
;; Exercise 24.0.7

;; valid
(check-expect ((lambda (x y) (x y y)) + 5) 10)
;; valid
(check-expect ((lambda () 10)) 10)
;; valid
(check-expect ((lambda (x) x) 5) 5)
;; valid
(check-expect ((lambda (x y) x) 5 10) 5)
;; invalid
;; (lambda x 10)


(test)
