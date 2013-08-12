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

;; Exercise 2.2.5
(define (square-plus-10 n)
        (+ (sqr n) 10))
(square-plus-10 2)
(square-plus-10 9)

(define (square-plus-20 n)
        (+ (* (sqr n) (/ 1 2))
           20))
(square-plus-20 2)
(square-plus-20 9)

(define (two-minus-n-reverse n)
        (- 2
           (/ 1 n)))
(two-minus-n-reverse 2)
(two-minus-n-reverse 9)

;; Exercise 2.3.1
(define (wage h)
  (* h 12))

(define (tax grosspay)
  (* 0.15 grosspay))

(tax 2000)

(define (netpay hour)
  (* (- 1 0.15) (wage hour)))

(netpay 40)

;; Exercise 2.3.2
(define (sum-coins pennies nickels dimes quarters)
  (+ pennies
     (* 5 nickels)
     (* 10 dimes)
     (* 25 quarters)))

(sum-coins 34 432 23 1232)

;; Exercise 2.3.3
(define (total-profit attendees)
  (- (* 5 attendees)
     (+ 20 (* 0.5 attendees))))

(total-profit 100)
