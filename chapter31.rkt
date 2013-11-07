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

(test)
