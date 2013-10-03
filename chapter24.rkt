#lang racket
(require test-engine/racket-tests)
;; Exercise 24.0.7

;; valid
(check-expect ((lambda (x y) (x y y)) + 5) 10)
;; valid
(check-expect ((lambda () 10)) 10)
;; valid
(check-expect ((lambda (x) x) 5) 5)
;; valid
(check-expect ((lambda (x y) x) 5 10) 5)
;; invalid
;; (lambda x 10)

;; Exercise 24.0.8
(check-expect ((lambda (x y)
                       (+ x (* x y))) 3 4)
              15)
(check-expect ((lambda (x y)
                      (+ x
                         (local ((define x (* y y)))
                                (+ (* 3 x)
                                   (/ 1 x)))))
                 3 4)
              817/16)
(check-expect ((lambda (x y)
                       (+ x
                          ((lambda (x)
                                   (+ (* 3 x)
                                      (/ 1 x)))
                             (* y y))))
                 3 4)
              817/16)

;; Exercise 24.0.9
(check-expect ((lambda (x y)
                       (+ x (* x y)))
                 1 2)
              3)
(check-expect ((lambda (x y)
                      (+ x
                         (local ((define x (* y y)))
                                (+ (* 3 x)
                                   (/ 1 x)))))
                 1 2)
              53/4)
(check-expect ((lambda (x y)
                       (+ x
                          ((lambda (x)
                                   (+ (* 3 x)
                                      (/ 1 x)))
                             (* y y))))
                 1 2)
              53/4)
(test)
