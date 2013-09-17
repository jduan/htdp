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
