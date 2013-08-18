;; Exercise 10.1.1
;; calculate the total wage of someone who worked for h hours
(define (wage h)
  (* 14 h))
;; calculate the wages for a given list of hours
(define (hours->wages a-list-of-hours)
  (cond
    [(empty? a-list-of-hours) empty]
    [else (cons (wage (first a-list-of-hours))
                (hours->wages (rest a-list-of-hours)))]))
(equal? '(140 280 420) (hours->wages '(10 20 30)))
(equal? '(28 196 2954) (hours->wages '(2 14 211)))

