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
