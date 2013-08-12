;; Exercise 3.1.2
;; maximize profit of movie theater

(define (price n)
  (- 5 (* 0.1 n)))

(define (num-of-people n)
  (+ 120 (* 15 n)))

(define (cost n)
  (+ 180 (* 0.04 (num-of-people n))))

(define (revenue n)
  (* (price n) (num-of-people n)))

(define (profit n)
  (- (revenue n) (cost n)))

(profit 0)
(profit 10)
(profit 20)
(profit 30)
(profit 40)
(profit 50)

;; Exercise 3.1.2 version 2

(define (profit ticket-price)
  (- (revenue ticket-price) (cost ticket-price)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ 180 (* 0.04 (attendees ticket-price))))

(define (attendees ticket-price)
  (+ 120
     (* 15 (/ (- 5 ticket-price) 0.1))))

(profit 0)
(profit 1)
(profit 2)
(profit 3)
(profit 4)
(profit 5)
