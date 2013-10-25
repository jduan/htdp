#lang racket
(require test-engine/racket-tests)

;; Exercise 28.1.1
(check-expect
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ()))
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D empty)
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G empty)))



(test)
