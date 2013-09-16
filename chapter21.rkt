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

;; Exercise 21.1.2
;; sum : (listof number) -> number
;; to compute the sum of a list of numbers
(define (sum alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum (rest alon)))]))

(equal? (sum '(1 2 3 4 5)) 15)

;; product : (listof number) -> number
;; to compute the product of a list of numbers
(define (product alon)
  (cond
    [(empty? alon) 1]
    [else (* (first alon) (product (rest alon)))]))

(equal? (product '(1 2 3 4 5)) 120)

;; fold: (listof X) init f ->
(define (fold lst init acc)
  (cond
    [(empty? lst) init]
    [else (acc (first lst) (fold (rest lst) init acc))]))

(define (sum1 alon)
  (fold alon 0 +))

(equal? (sum1 '(1 2 3 4 5)) 15)

(define (product1 alon)
  (fold alon 1 *))

(equal? (product1 '(1 2 3 4 5)) 120)

(define (append lst1 lst2)
  (fold lst1 lst2 cons))

(equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (map2 lst f)
  (fold lst empty (lambda (x l) (cons (f x) l))))

(equal? (map2 '(1 2 3) add1) '(2 3 4))

;; Exercise 21.1.3
(define (natural-f n obj f init)
  (cond
    [(zero? n) init]
    [else (f obj (natural-f (sub1 n) obj f init))]))

(define (copy n obj)
  (natural-f n obj cons empty))

(equal? (copy 5 'hello) '(hello hello hello hello hello))

(define (n-adder n x)
  (natural-f n 1 + x))

(equal? (n-adder 10 5) 15)
(equal? (n-adder 100 5) 105)

(define (n-multiplier n x)
  (natural-f n x + 0))

(equal? (n-multiplier 10 5) 50)
(equal? (n-multiplier 3 5) 15)

(equal? (build-list 10 identity) '(0 1 2 3 4 5 6 7 8 9))
(equal? (filter even? (build-list 10 identity))
        '(0 2 4 6 8))
(equal? (andmap even? (build-list 10 identity)) false)
(equal? (andmap number? (build-list 10 identity)) true)
(equal? (ormap symbol? (build-list 10 identity)) false)
(equal? (ormap symbol? (cons 'hello (build-list 10 identity))) true)
(equal? (foldr + 100 (build-list 10 identity)) 145)
(equal? (foldl + 100 (build-list 10 identity)) 145)

;; Exercise 21.2.1
(equal? (list 0 1 2 3) (build-list 4 identity))
(equal? (list 1 2 3 4) (build-list 4 add1))
(equal? (list .1 .01 .001 .0001) (build-list 4 (lambda (x) (expt 0.1 (add1 x)))))
(define (evens n)
  (build-list n (lambda (x) (* 2 x))))

(equal? (evens 2) '(0 2))
(equal? (evens 5) '(0 2 4 6 8))
;; To tabulate an operation between n and 0 (inclusive) n a list
(define (tabulate f n)
  (reverse (build-list (add1 n) f)))

(equal? (tabulate identity 10) '(10 9 8 7 6 5 4 3 2 1 0))
(equal? (tabulate identity 4)  (list 4 3 2 1 0))
(equal? (tabulate sin 4)  (list  (sin 4)  (sin 3)  (sin 2)  (sin 1)  (sin 0)))

;; n -> (listof (listof 0 or 1))
(define (diagonal n)
  (local ((define (build-row i)
            (build-list n (lambda (x) (if (= i x) 1 0)))))
         (build-list n build-row)))

(equal? (diagonal 3)
        (list
          (list 1 0 0)
          (list 0 1 0)
          (list 0 0 1)))
