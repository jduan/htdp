#lang racket
(require test-engine/racket-tests)
;; Exercise 23.1.1
(define (series-local a-term)
  (local [(define (series n)
            (cond
              [(zero? n) (a-term n)]
              [else (+ (a-term n)
                       (series (sub1 n)))]))]
         series))

;; Return the n-th even number, index is 0 based.
(define (make-even n)
  (* n 2))

;; Return the n-th odd number, index is 0 based.
(define (make-odd n)
  (add1 (* n 2)))

(define series-even2 (series-local make-even))
(define series-odd2 (series-local make-odd))

(check-expect (series-even2 9) 90)
(check-expect (series-odd2 9) 100)

(test)
