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

;; Exercise 20.1.3
;; Develop a-function=?. The function determines whether two functions from
;; numbers to numbers produce the same results for 1.2, 3, and -5.7.
(define (a-function=? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 -5.7) (f2 -5.7))))

(equal? (a-function=? identity identity) true)
(equal? (a-function=? sub1 identity) false)
