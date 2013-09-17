;; Exercise 22.2.1
;; (X -> y) -> ((listof X) -> (listof Y))
;; Consumes a convert function and returns a function that takes a list of
;; items and applies the convert function.
(define (mymap converter)
  (local ((define (abs-fun lst)
            (map converter lst)))
         abs-fun))

(define (C->F degree) (+ 32 (* degree 9/5)))
((mymap C->F) '(0 32 100))

(define-struct ir (name price))
(define inventory-records
  (list (make-ir 'book 25)
        (make-ir 'video 5)
        (make-ir 'audio 7)
        (make-ir 'ce 100)))
(equal? '(book video audio ce)((mymap ir-name) inventory-records))

;; Exercise 22.2.2
;; (X Y -> boolean) -> ((listof X) -> (listof X))
;; Consumes a comparison function and returns a function that takes a list of
;; items and sort them using the comparison function.
(define (mysort cmp)
  (local ([define (mysort alon)
           (cond
             [(empty? alon) alon]
             [else (insert (first alon) (mysort (rest alon)))])]
          [define (insert an alon)
           (cond
             [(empty? alon) (list an)]
             [else (cond
                     [(cmp an (first alon)) (cons an alon)]
                     [else (cons (first alon) (insert an (rest alon)))])])])
         mysort))

(equal? ((mysort <) '(2 3 1 5 4)) '(1 2 3 4 5))
(equal? ((mysort >) '(2 3 1 5 4)) '(5 4 3 2 1))

;; Exercise 22.2.3
;; (X Y ... Z -> Z') init -> ((listof X) -> value)
;; Consumes a combine function and an initial value, and returns a function that
;; takes a list of items and "fold" them into a final value.
;; my own implementation of fold right
(define (myfoldr combine init)
  (local ([define (myfoldr lst)
           (cond
             [(empty? lst) init]
             [else (combine (first lst) (myfoldr (rest lst)))])])
         myfoldr))

(equal? ((myfoldr + 0) '(1 2 3 4)) 10)
(equal? ((myfoldr - 0) '(1 2 3 4)) -2)
(equal? ((myfoldr * 1) '(1 2 3 4)) 24)
(equal? ((myfoldr / 1) '(1 2 3 4)) 3/8)
(equal? ((myfoldr cons '(1 2 3)) '(4 5 6)) '(4 5 6 1 2 3))

;; my own implementation of fold left
(define (myfoldl combine init)
  (local ([define (myfoldl lst acc)
           (cond
             [(empty? lst) acc]
             [else (myfoldl (rest lst) (combine (first lst) acc))])]
          [define (myfoldl2 lst)
           (myfoldl lst init)])
          myfoldl2))

(equal? ((myfoldl - 0) '(1 2 3 4)) 2)
(equal? ((myfoldl / 1) '(1 2 3 4)) 8/3)
(equal? ((myfoldl cons '(1 2 3)) '(4 5 6)) '(6 5 4 1 2 3))
