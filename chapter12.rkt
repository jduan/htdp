;; Insert a number into a list of sorted numbers
(define (insert-to-sorted-list n alon)
  (if (empty? alon)
    (cons n empty)
    (if (< n (first alon))
      (cons n alon)
      (cons (first alon) (insert-to-sorted-list n (rest alon))))))

;; Sort a list of numbers in ascending order
(define (mysort alon)
  (if (empty? alon)
    empty
    (insert-to-sorted-list
      (first alon)
      (mysort (rest alon)))))

(equal? '(1 2 3 4 5 6) (mysort '(3 4 2 1 6 5)))

;; Exercise 12.2.1
(define-struct mail (from data message))
