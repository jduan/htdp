#lang racket
(require test-engine/racket-tests)

;; Exercise 26.1.1
(define (tabulate-div num)
  (define (tabulate current)
    (cond
      [(= current num) (list num)]
      [else
       (cond
         [(zero? (remainder num current))
          (cons current (tabulate (add1 current)))]
         [else (tabulate (add1 current))])]))
  (tabulate 1))

(check-expect (tabulate-div 10) (list 1 2 5 10))
(check-expect (tabulate-div 20) (list 1 2 4 5 10 20))

;; Exercise 26.1.2
(define (make-singles lst)
  (map list lst))

(check-expect (make-singles '(2 5 9 3)) '((2) (5) (9) (3)))
(check-expect (make-singles empty) empty)

(define (merge-two-lists lst1 lst2)
  (cond
    [(empty? lst2) lst1]
    [(empty? lst1) lst2]
    [(<= (first lst1) (first lst2))
     (cons (first lst1) (merge-two-lists (rest lst1) lst2))]
    [else
     (cons (first lst2) (merge-two-lists lst1 (rest lst2)))]))

(check-expect (merge-two-lists '(2 5) '(3 9)) '(2 3 5 9))

(define (merge-all-neighbors list-of-lists)
  (cond
    [(empty? list-of-lists) empty]
    [(= 1 (length list-of-lists)) list-of-lists]
    [else
     (cons (merge-two-lists (first list-of-lists) (second list-of-lists))
           (merge-all-neighbors (rest (rest list-of-lists))))]))

(check-expect (merge-all-neighbors (list (list 2) (list 5) (list 9) (list 3)))
              (list (list 2 5) (list 3 9)))
(check-expect (merge-all-neighbors (list (list 2 5) (list 3 9)))
              (list (list 2 3 5 9)))

(define (merge-sort lst)
  (define (merge-helper list-of-lists)
    (cond
      [(empty? list-of-lists) empty]
      [(= 1 (length list-of-lists)) list-of-lists]
      [else (merge-helper (merge-all-neighbors list-of-lists))]))
  (let* ([list-of-lists (make-singles lst)]
         [result (merge-helper list-of-lists)])
    (cond
      [(empty? result) empty]
      [else (first result)])))

(check-expect (merge-sort '(2 5 9 3)) '(2 3 5 9))
(check-expect (merge-sort '()) '())
(check-expect (merge-sort (list)) (list))
(check-expect (merge-sort (list 2 5 9 3)) (list 2 3 5 9))
(check-expect (merge-sort (list 2 5 9 2 3)) (list 2 2 3 5 9))
(test)
