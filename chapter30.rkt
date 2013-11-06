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

(define SimpleG '((A B)
                  (B C)
                  (C E)
                  (D E)
                  (E B)
                  (F F)))
(define (next-node node sg)
    (second (assoc node sg)))

(define (route-exists? orig dest sg)
  (cond
    [(symbol=? orig dest) true]
    [else (route-exists? (next-node orig sg) dest sg)]))

(check-expect (route-exists? 'A 'E SimpleG) true)
(check-expect (route-exists? 'A 'C SimpleG) true)

(define (contains sym lst)
  (member sym lst))

(define (route-exists2? orig dest sg)
  (define (re-accu? orig accu-seen)
    (cond
      [(symbol=? orig dest) true]
      [(contains orig accu-seen) false]
      [else (re-accu? (next-node orig sg) (cons orig accu-seen))]))
  (re-accu? orig empty))

(check-expect (route-exists2? 'A 'E SimpleG) true)
(check-expect (route-exists2? 'A 'C SimpleG) true)
(check-expect (route-exists2? 'A 'D SimpleG) false)
(check-expect (route-exists2? 'B 'D SimpleG) false)


(test)
