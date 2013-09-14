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

;; Exercise 18.1.5
(equal? 30 (local ((define (x y) (* 3 y)))
                  (* (x 2) 5)))

(equal? -18 (local ((define (f c) (+ (* 9/5 c) 32)))
                   (- (f 0) (f 10))))

(equal? false (local [(define (odd? n)
                        (cond
                          [(zero? n) false]
                          [else (even? (sub1 n))]))
                      (define (even? n)
                        (cond
                          [(zero? n) true]
                          [else (odd? (sub1 n))]))]
                     (even? 1)))

(equal? 588 (+ (local ((define (f x) (g (+ x 1) 22))
                         (define (g x y) (+ x y)))
                      (f 10))
               555))

(define (h n)
  (cond
    [(= n 0) empty]
    [else (local ((define r (* n n)))
                 (cons r (h (- n 1))))]))
(equal? '(4 1) (h 2))
