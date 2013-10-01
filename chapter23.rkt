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

;; Exercise 23.2.1
(define (a-fives n)
  (cond
    [(zero? n) 8]
    [else (+ 5 (a-fives (sub1 n)))]))

(check-expect (a-fives 0) 8)
(check-expect (a-fives 1) 13)
(check-expect (a-fives 2) 18)

;; Exercise 23.2.2
(define (a-fives-closed n)
  (+ 3 (* 5 (add1 n))))
(check-expect (a-fives-closed 0) 8)
(check-expect (a-fives-closed 1) 13)
(check-expect (a-fives-closed 2) 18)

;; Exercise 23.2.3
(define sum-of-a-fives (series-local a-fives))
(check-expect (sum-of-a-fives 3) 62)
(check-expect (sum-of-a-fives 7) 204)
(check-expect (sum-of-a-fives 88) 20292)
(test)
