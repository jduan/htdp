#lang racket

(define (area-of-disk r)
  (* 3.14 (sqr r)))

(area-of-disk 5)

(define (area-of-ring outter-radius inner-radius)
  (- (area-of-disk outter-radius)
     (area-of-disk inner-radius)))

(area-of-ring 5 3)

;; Exercise 2.2.1
(define (Fahrenheit->Celsius n)
  (* (/ 5 9) (- n 32)))

(Fahrenheit->Celsius 72)

;; Exercise 2.2.2
(define (dollar->euro amount)
  (* 0.75 amount))

(dollar->euro 100)

;; Exercise 2.2.3
(define (triangle length height)
  (* 0.5 length height))

(triangle 3 4)

;; Exercise 2.2.4
(define (convert3 lsb1 lsb2 lsb3)
  (+ lsb1
     (* 10 lsb2)
     (* 100 lsb3)))

(convert3 1 2 3)
