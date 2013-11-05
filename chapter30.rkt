#lang racket
(require test-engine/racket-tests)

;; Exercise 30.1.1
(define (relative-to-absolute alon)
  (define (add-to-each n alon)
    (map (lambda (i) (+ i n)) alon))
  (cond
    [(empty? alon) empty]
    [else (cons (first alon)
                (add-to-each (first alon) (relative-to-absolute (rest alon))))]))

(check-expect (relative-to-absolute (list 50 40 70 30 30)) (list 50 90 160 190 220))

(define (relative-to-absolute2 alon)
  (define (helper alon acc)
    (cond
      [(empty? alon) empty]
      [else (cons (+ acc (first alon))
                  (helper (rest alon) (+ (first alon) acc)))]))
  (helper alon 0))

(check-expect (relative-to-absolute2 (list 50 40 70 30 30)) (list 50 90 160 190 220))
(test)
