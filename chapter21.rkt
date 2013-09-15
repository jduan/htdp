;; Exercise 21.1.1
(define (tabulate-sin n)
  (cond
    [(zero? n) (list (sin 0))]
    [else (cons (sin n) (tabulate-sin (sub1 n)))]))

(tabulate-sin 5)

(define (tabulate-sqrt n)
  (cond
    [(zero? n) (list (sqrt 0))]
    [else (cons (sqrt n) (tabulate-sqrt (sub1 n)))]))

(tabulate-sqrt 5)

;; To tabulate an operation between n and 0 (inclusive) n a list
(define (tabulate f n)
  (cond
    [(zero? n) (list (f 0))]
    [else (cons (f n) (tabulate f (sub1 n)))]))

(define (tabulate-sin1 n)
  (tabulate sin n))

(define (tabulate-sqrt1 n)
  (tabulate sqrt n))

(equal? (tabulate-sin 5) (tabulate-sin1 5))
(equal? (tabulate-sqrt 5) (tabulate-sqrt1 5))

(define (tabulate-sqr n)
  (tabulate sqr n))

(define (tabulate-tan n)
  (tabulate tan n))
