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

(struct node (left right))

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (max (height (node-left tree)) (height (node-right tree))))]))

(define (height2 tree)
  (define (helper tree acc)
    (cond
      [(empty? tree) acc]
      [else (max (helper (node-left tree) (add1 acc))
                 (helper (node-right tree) (add1 acc)))]))
  (helper tree 0))

;; Exercise 31.3.3
(define (product lon)
  (define (product/acc lon acc)
    (cond
      [(empty? lon) acc]
      [else (product/acc (rest lon) (* acc (first lon)))]))
  (product/acc lon 1))

(check-expect (product (list 1 2 3 4 5)) 120)

;; Exercise 31.3.4
(define (how-many xs)
  (define (how-many/acc xs acc)
    (cond
      [(empty? xs) acc]
      [else (how-many/acc (rest xs) (add1 acc))]))
  (how-many/acc xs 0))

(check-expect (how-many (list 1 2 3 4 5)) 5)
(check-expect (how-many empty) 0)

;; Exercise 31.3.5
(define (add-to-pi n)
  (define (add-to-pi/acc n acc)
    (cond
      [(zero? n) acc]
      [else (add-to-pi/acc (sub1 n) (add1 acc))]))
  (add-to-pi/acc n pi))

(check-within (add-to-pi 3) (+ pi 3) 0.00001)
(check-within (add-to-pi 0) (+ pi 0) 0.00001)

(define (add n x)
  (define (add/acc n acc)
    (cond
      [(zero? n) acc]
      [else (add/acc (sub1 n) (add1 acc))]))
  (add/acc n x))

(check-within (add 3 10.0) 13 0.0001)

;; Exercise 31.3.6
(define (make-palindrome xs)
  (define (make-palindrome/acc xs acc)
    (cond
      [(= 1 (length xs)) acc]
      [else (make-palindrome/acc (rest xs)
                                 (cons (first xs) acc))]))
  (append xs (make-palindrome/acc xs empty)))

(check-expect (make-palindrome '(a b c)) '(a b c b a))
(check-expect (make-palindrome '(a b c d e)) '(a b c d e d c b a))


;; Exercise 31.3.7
(define (to10 lod)
  (define (to10/acc lod acc)
    (cond
      [(empty? lod) acc]
      [else (to10/acc (rest lod) (+ (* 10 acc) (first lod)))]))
  (to10/acc lod 0))

(check-expect (to10 (list 1 0 2)) 102)
(check-expect (to10 empty) 0)

(define (to10-general base lod)
  (define (to10-general/acc lod acc)
    (cond
      [(empty? lod) acc]
      [else (to10-general/acc (rest lod) (+ (* base acc) (first lod)))]))
  (to10-general/acc lod 0))

(check-expect (to10-general 10 (list 1 0 2)) 102)
(check-expect (to10-general 8 (list 1 0 2)) 66)

;; Exercise 31.3.8
(define (is-prime? n)
  (define (is-prime/acc? i acc)
    (cond
      [acc false]
      [(= i 1) true]
      [else (is-prime/acc? (sub1 i) (zero? (remainder n i)))]))
  (is-prime/acc? (sub1 n) true))

(check-expect (is-prime? 3) false)
(test)
