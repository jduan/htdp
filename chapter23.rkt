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

;; Exercise 23.3.4
(define (geometric-series start s)
  (lambda (n)
          (* start (expt s n))))

(check-expect ((sequence2 (geometric-series 3 5)) 5) '(3 15 75 375 1875))

;; Exercise 23.3.5
(define sum-of-g-fives (series-local g-fives))
(check-expect (sum-of-g-fives 3) 468)
(check-expect (sum-of-g-fives 7) 292968)
(check-expect (sum-of-g-fives 88) 121169035041947413311241507627435964877804508432745933532714843)

(define sum-of-g-series (series-local (geometric-series 1 .1)))
(check-within(sum-of-g-series 3) 1.111 0.00001)
(check-within (sum-of-g-series 7) 1.1111111 0.00000000001)
(check-within (sum-of-g-series 88) 1.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
              (expt 0.1 100))

;; Exercise 23.3.6
(define (e-power x)
  (define (factorial n)
    (if (zero? n) 1 (* n (factorial (sub1 n)))))
  (define (e-tylor i)
    (/ (expt x i) (factorial i)))
  (exact->inexact ((series-local e-tylor) 10)))
(e-power 1)

;; Exercise 23.3.7
(define (ln x)
  (define (e-tylor i)
    (if (even? i) 0
      (* 2 (/ 1 i) (expt
                     (/ (sub1 x) (add1 x))
                     i))))
  (exact->inexact ((series-local e-tylor) 1000)))
(ln 10)

;; Exercise 23.3.8
(define (my-sin x)
  (define (factorial n)
    (if (zero? n) 1 (* n (factorial (sub1 n)))))
  (define (e-tylor i)
    (let ([index (add1 (* i 2))]
          [sign (if (even? i) 1 -1)])
      (* sign (/ (expt x index) (factorial index)))))
  (exact->inexact ((series-local e-tylor) 100)))
(my-sin 10)

;; Exercise 23.3.9
(define (my-pi)
  (define (greg i)
    (let ([index (add1 (* i 2))]
          [sign (if (even? i) 1 -1)])
      (* sign (/ 4 index))))
  (exact->inexact ((series-local greg) 1000)))
(my-pi)

;; TODO: read sections 23.4 and 23.5
(test)
