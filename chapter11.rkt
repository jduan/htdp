(define (hellos n)
  (if (zero? n)
    empty
    (cons 'hello (hellos (sub1 n)))))
(equal? '(hello hello hello) (hellos 3))
(equal? '(hello ) (hellos 1))
(equal? '() (hellos 0))

;; Exercise 11.2.1
(define (repeat n sym)
  (if (zero? n)
    empty
    (cons sym (repeat (sub1 n) sym))))
(equal? '(haha haha haha) (repeat 3 'haha))

;; Exercise 11.2.2
(define (f x)
  (+ (* 3 (* x x))
     (+ (* -6 x)
        -1)))
(define (tabulate-f n)
  (if (zero? n)
    empty
    (cons (make-posn n (f n))
          (tabulate-f (sub1 n)))))
(equal? (cons (make-posn 4 23)
              (cons (make-posn 3 8)
                    (cons (make-posn 2 -1)
                          (cons (make-posn 1 -4)
                                empty))))
        (tabulate-f 4))

;; Exercise 11.2.4
(define (make-deep-list sym n)
  (if (zero? n)
    sym
    (cons (make-deep-list sym (sub1 n)) empty)))
(equal? 'hello (make-deep-list 'hello 0))
(equal? '(((hello))) (make-deep-list 'hello 3))

(define (depth dlist)
  (if (symbol? dlist)
    0
    (add1 (depth (first dlist)))))
(equal? 0 (depth (make-deep-list 'hello 0)))
(equal? 3 (depth (make-deep-list 'hello 3)))
(equal? 5 (depth (make-deep-list 'hello 5)))

(define (random-n-m n m)
  (+ (random (- m n)) n))

;; Exercise 11.3.2
(define (tie-dyed n)
  (if (zero? n)
    empty
    (cons (random-n-m 20 120) (tie-dyed (sub1 n)))))
(tie-dyed 5)

;; Factorial
(define (! n)
  (if (zero? n)
    1
    (* n (! (sub1 n)))))
(equal? 3628800 (! 10))

;; Exercise 11.4.2
(define (product n m)
  (if (= n m)
    1
    (* (product n (sub1 m)) m)))
(equal? 30240 (product 5 10))
(equal? 10 (product 9 10))

(define (product-from-20 n)
  (product 20 n))
(equal? (product-from-20 30) 109027350432000)

(define (add-to-pie n)
  (if (zero? n)
    pi
    (add1 (add-to-pie (sub1 n)))))
(equal? (+ pi 3) (add-to-pie 3))

;; Exercise 11.5.1
(define (add n x)
  (if (zero? n)
    x
    (add (sub1 n) (add1 x))))
(equal? (add 10 20) 30)
(equal? (add 10 0) 10)
(equal? (add 0 10) 10)
(equal? (add 10 10) 20)

(define (multiply n x)
  (cond
    [(= n 0) 0]
    [else (add x (multiply (sub1 n) x))]))
(equal? 15 (multiply 5 3))

(define (exponent n x)
  (if (zero? n)
    1
    (multiply x (exponent (sub1 n) x))))
(equal? (exponent 10 2) 1024)

(define (addDL n m)
  (if (symbol? n)
    m
    (addDL (first n) (cons m empty))))
(equal? 15 (depth (addDL (make-deep-list 'hello 5)
                        (make-deep-list 'hello 10))))
