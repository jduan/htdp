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

;; Exercise 18.1.3
(define A-CONSTANT
  (not
    (local [(define (odd an)
              (cond
                [(= an 0) false]
                [else (even (- an 1))]))
            (define (even an)
              (cond
                [(= an 0) true]
                [else (odd (- an 1))]))]
           (even 3))))

(+ (local [(define (f x) (+ (* x x) (* 3 x) 15))
           (define x 100)
           (define f@100 (f x))]
          f@100)
   1000)

(local ((define CONST 100)
          (define (f x) (+ x CONST)))
       (define (g x y z) (f (+ x (* y z)))))

;; Exercise 18.1.4
;; Impossible. Locals are locals.
