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
    [else (or (symbol=? 'doll (first a-list-of-symbols))
              (contains-doll? (rest a-list-of-symbols)))]))

(contains-doll? empty)
(contains-doll? '(ball))
(contains-doll? '(arrow doll))
(contains-doll? '(bow arrow ball))

;; Exercise 9.3.3
;; check if sym occurrs on the list
(define (contains? sym lst)
  (cond
    [(empty? lst) false]
    [else (or (symbol=? sym (first lst))
              (contains? sym (rest lst)))]))
(contains? 'doll empty)
(contains? 'doll (cons 'doll empty))
(contains? 'doll (cons 'rocket empty))

;; Exercise 9.5.1
(define (sum a-list-of-nums)
  (cond
    [(empty? a-list-of-nums) 0]
    [else (+ (first a-list-of-nums)
             (sum (rest a-list-of-nums)))]))
(= 0 (sum empty))
(= 1 (sum (cons 1.00 empty)))
(= 20.86 (sum '(17.05 1.22 2.59)))

;; Exercise 9.5.2
(define (how-many-symbols lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (how-many-symbols (rest lst)))]))
(= 3 (how-many-symbols '(hello world everyone)))

;; Exercise 9.5.3
(define (all-below? lst threshold)
  (cond
    [(empty? lst) true]
    [else (and (< (first lst) threshold)
               (all-below? (rest lst) threshold))]))
(define (dollar-store? lst)
  (all-below? lst 1))
(dollar-store? '(0.99 0.98 0.50))
(dollar-store? empty)
(not (dollar-store? '(0.75 1.95 0.25)))
(dollar-store? '(0.15 0.05 0.25))

;; Exercise 9.5.4
(define (check-range1 lst)
  (cond
    [(empty? lst) true]
    (else (and (<= 5 (first lst) 95)
               (check-range1 (rest lst))))))

(check-range1 empty)
(check-range1 '(50))
(check-range1 '(101))
(check-range1 '(45 52 23 44 96 22))

