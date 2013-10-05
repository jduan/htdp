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

(test)
