#lang racket
(require test-engine/racket-tests)

(define (invert lst)
  (define (invert-acc xs acc)
    (cond
      [(empty? xs) acc]
      [else (invert-acc (rest xs) (cons (first xs) acc))]))
  (invert-acc lst empty))

(check-expect (invert (list 1 2 3)) (list 3 2 1))
(check-expect (invert (list )) (list ))

(define (sum xs)
  (cond
    [(empty? xs) 0]
    [else (+ (first xs) (sum (rest xs)))]))

(check-expect (sum (list 1 2 3)) 6)
(check-expect (sum (list )) 0)

(define (sum2 xs)
  (define (sum-acc xs acc)
    (cond
      [(empty? xs) acc]
      [else (sum-acc (rest xs) (+ acc (first xs)))]))
  (sum-acc xs 0))

(check-expect (sum2 (list 1 2 3)) 6)
(check-expect (sum2 (list )) 0)

;; Exercise 31.3.1
(define (g-series n)
  (cond
    [(zero? n) empty]
    [else (cons (expt -0.99 n) (g-series (sub1 n)))]))

(check-within (sum (g-series #i1000)) -0.49746596003269394 0.001)
(check-within (sum2 (g-series #i1000)) -0.49746596003269533 0.001)
(check-within (* 10e15 (sum (g-series #i1000))) -4974659600326939.0 0.001)
(check-within (* 10e15 (sum2 (g-series #i1000))) -4974659600326953.0 0.001)

(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

(check-expect (! 3) 6)
(check-expect (! 5) 120)

(define (!2 n)
  (define (helper n acc)
    (cond
      [(zero? n) acc]
      [else (helper (sub1 n) (* n acc))]))
  (helper n 1))

(check-expect (!2 3) 6)
(check-expect (!2 5) 120)

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (max (height (node-left tree)) (height (node-right tree))))]))

(define (height2 tree)
  (define (helper tree acc)
    [(empty? tree) acc]
    [else (max (helper (node-left tree) (add1 acc))
               (helper (node-right tree) (add1 acc)))]))
(test)
