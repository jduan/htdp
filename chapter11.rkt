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
