;; Exercise 18.1.1
(local [(define (f x) (+ x 5))
        (define (g alon)
          (cond
            [(empty? alon) empty]
            [else (cons (f (first alon)) (g (rest alon)))]))]
       (g (list 1 2 3)))

(local [(define (odd an)
          (cond
            [(zero? an) false]
            [else (even (sub1 an))]))
        (define (even an)
          (cond
            [(zero? an) true]
            [else (odd (sub1 an))]))]
       (even 12))

;; Exercise 18.1.2
(local [(define (f x) (+ (* x x) (* 3 x) 15))
        (define x 100)
        (define f@100 (f x))]
       f@100 x )

(local ((define (f x) (+ (* x x) (* 3 x) 14))
          (define x 100)
          (define g (f x)))
       g)
