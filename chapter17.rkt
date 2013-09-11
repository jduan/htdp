;; construct a new list by replacing empty in alon1 by alon2
(define (replace-eol-with alon1 alon2)
  (cond
    [(empty? alon1) alon2]
    [else (cons (first alon1)
                (replace-eol-with (rest alon1) alon2))]))

(equal? (replace-eol-with (list 1 2 3) (list 4 5 6))
        (list 1 2 3 4 5 6))
(equal? (replace-eol-with empty (list 4 5 6))
        (list 4 5 6))
