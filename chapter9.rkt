;; Exercise 9.1.3
(define (add-up-3 a-list-of-3-numbers)
  (+ (first a-list-of-3-numbers)
     (first (rest a-list-of-3-numbers))
     (first (rest (rest a-list-of-3-numbers)))))
(= 6 (add-up-3 '(1 2 3)))
(= 126 (add-up-3 '(7 41 78)))

(define (distance-to-0-for-2 x y)
  (sqrt (+ (sqr x) (sqr y))))
(= 5 (distance-to-0-for-2 3 4))

(define (distance-to-0-for-3 lst)
  (distance-to-0-for-2 (first lst)
                       (distance-to-0-for-2 (first (rest lst))
                                            (first (rest (rest lst))))))
(close-enough? (distance-to-0-for-3 '(3 4 12)) 13)
(close-enough? (distance-to-0-for-3 '(1 1 1)) (sqrt 3))
(close-enough? (distance-to-0-for-3 '(-1 2 -1)) (sqrt 6))

;; Exercise 9.1.4
(define (contains-2-doll? lst)
  (or (symbol=? 'doll (first lst))
      (symbol=? 'doll (first (rest lst)))))
(contains-2-doll? '(doll what))
(contains-2-doll? '(hello what))

;; Exercise 9.3.1
(define (contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (cond
            [(symbol=? 'doll (first a-list-of-symbols)) true]
            [else (contains-doll? (rest a-list-of-symbols))])]))

(contains-doll? empty)
(contains-doll? '(ball))
(contains-doll? '(arrow doll))
(contains-doll? '(bow arrow ball))

