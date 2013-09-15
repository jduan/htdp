;; Exercise 20.1.1
(define (f x) x)

(equal? (list f) (cons f empty))
(equal? f (f f))
(equal? (list f 10 10) (cons f (cons 10 (cons (f 10) empty))))

;; Exercise 20.1.2
(define (f x) (x 10))
(equal? (f add1) 11)

(define (f x) f)
(equal? f (f 100))

(define (f x y) (x 'a y 'b))
(equal? (f list 'c) '(a c b))
