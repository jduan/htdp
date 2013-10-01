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

;; Exercise 23.2.4
(define (seq-a-fives n)
  (define (seq-reversed n)
    (cond
      [(zero? n) (list (a-fives n))]
      [else (cons (a-fives n) (seq-reversed (sub1 n)))]))
  (reverse (seq-reversed n)))

(check-expect (seq-a-fives 10) '(8 13 18 23 28 33 38 43 48 53 58))

;; Exercise 23.2.5
(define (arithmetic-series start s)
  (lambda (n)
          (+ start (* s (add1 n)))))

(check-expect ((arithmetic-series 3 5) 10) (a-fives 10))
(check-expect ((arithmetic-series 0 2) 10) (make-even 11))

;; Exercise 23.3.1
(define (g-fives n)
  (cond
    [(zero? n) 3]
    [else (* 5 (g-fives (sub1 n)))]))

(define (sequence a-term)
  (define (seq-reversed n)
    (cond
      [(zero? n) (list (a-term n))]
      [else (cons (a-term n) (seq-reversed (sub1 n)))]))
  (lambda (n)
          (reverse (seq-reversed n))))

(check-expect ((sequence g-fives) 4) '(3 15 75 375 1875))

(define (sequence2 a-term)
  (lambda (n)
          (map a-term (for/list ([x (in-range n)]) x))))

(check-expect ((sequence2 g-fives) 5) '(3 15 75 375 1875))

;; Exercise 23.3.2
(define (g-fives-closed n)
  (* 3 (expt 5 n)))

(check-expect ((sequence2 g-fives-closed) 5) '(3 15 75 375 1875))

(test)
