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

;; Exercise 3.1.4
(define (cost ticket-price)
  (* 1.5 (attendees ticket-price)))

;; Exercise 3.2.1
(define BASE-PRICE 5)
(define PRICE-DROP-UNIT 0.1)
(define BASE-ATTENDEES 120)
(define ATTENDEES-BY-PRICE-DROP 15)
(define BASE-COST 180)
(define COST-PER-ATTENDEE 0.04)

(define (price n)
  (- BASE-PRICE (* PRICE-DROP-UNIT n)))

(define (num-of-people n)
  (+ BASE-ATTENDEES (* ATTENDEES-BY-PRICE-DROP n)))

(define (cost n)
  (+ BASE-COST (* COST-PER-ATTENDEE (num-of-people n))))

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

;; Exercise 3.3.1
(define CM-PER-INCH 2.54)
(define INCH-PER-FOOT 12)
(define FEET-PER-YARD 3)
(define YARD-PER-ROD 5.5)
(define ROD-PER-FURLONG 40)
(define FURLONG-PER-MILE 8)

(define (inches->cm n)
  (* CM-PER-INCH n))
(define (feet->inches n)
  (* INCH-PER-FOOT n))
(define (yards->feet n)
  (* FEET-PER-YARD n))
(define (rods->yards n)
  (* YARD-PER-ROD n))
(define (furlongs->rods n)
  (* ROD-PER-FURLONG n))
(define (miles->furlongs n)
  (* FURLONG-PER-MILE n))

(define (feet->cm n)
  (* (feet->inches n) (inches->cm n)))
(define (yards->cm n)
  (* (yards->feet n) (feet->cm n)))
(define (rods->inches n)
  (* (rods->yards n) (yards->feet n) (feet->inches n)))
(define (miles->feet n)
  (* (miles->furlongs n)
     (furlongs->rods n)
     (rods->yards n)
     (yards->feet n)))

;; should be 5280
(miles->feet 1)
;; should be 91.44
(yards->cm 1)

;; Exercise 3.3.2
(define (area-of-disk radius)
  (* pi (sqr radius)))

(define (volume-cylinder radius height)
  (* (area-of-disk radius) height))

(volume-cylinder 5 10)
