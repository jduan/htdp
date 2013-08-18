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
