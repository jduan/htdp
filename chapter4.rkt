;; Exercise 4.1.1
(and (> 4 3) (<= 10 100))   ;; => true
(or (> 4 3) (= 10 100))     ;; => true
(not (= 2 3))               ;; => true

;; Exercise 4.1.2
(> x 3)   ;; true when x = 4
(> x 3)   ;; false when x = 2
(> x 3)   ;; true when x = 7/2

(and (> 4 x) (> x 3)) ;; false when x = 4
(and (> 4 x) (> x 3)) ;; false when x = 2
(and (> 4 x) (> x 3)) ;; true when x = 7/2

(= (* x x) x) ;; false when x = 4
(= (* x x) x) ;; false when x = 2
(= (* x x) x) ;; false when x = 7/2

;; Exercise 4.2.1
(define (greater-than-3-less-or-equal-to7 x)
  (and (> x 3) (<= 7)))

(define (between-3-and-7 x)
  (and (>= x 3) (<= x 7)))

(define (ge-3-l-9 x)
  (and (>= x 3) (< x 9)))

(define (union x)
  (or (and (> x 1) (< x 3))
      (and (> x 9) (< x 11))))

(define (outside x)
  (not (and (>= x 1) (<= x 3))))

;; Exercise 4.2.2
;; (-3 0)
;; ()
;; < 1 or > 5

;; Exercise 4.2.3
(define (equal-to-62 n)
  (= 62 (+ 2 (* 4 n))))
(equal-to-62 10)
(equal-to-62 12)
(equal-to-62 14)
(equal-to-62 15)

(define (equal-to-102 n)
  (= 102 (* 2 n n)))
(equal-to-102 10)
(equal-to-102 12)
(equal-to-102 14)

(define (equal-to-462 n)
  (= 462
     (+ (* 4 n n)
        (* 6 n)
        2)))
(equal-to-462 10)
(equal-to-462 12)
(equal-to-462 14)

;; Exercise 4.3.1
;; The one on the left is legal. The one on the right is not.
;; The following is legal because the 2nd clause is malformed.

;; Exercise 4.3.2
;; when n is 500, the result is 0.40
;; when n is 2800, the result is 0.45
;; when n is 15000, the result is 0.60

;; Exercise 4.3.3
;; when n is 500, the result is 40
;; when n is 2800, the result is 121
;; when n is 15000, the result is 495

;; Exercise 4.4.1

(define (interest amount)
  (cond
    ((<= amount 1000) (* 4/100 amount))
    ((<= amount 5000) (* 45/1000 amount))
    (else (* 5/100 amount))))

(interest 500)
(interest 2000)
(interest 10000)

;; Exercise 4.4.2

(define (tax gross-pay)
  (cond
    ((<= gross-pay 240) 0)
    ((<= gross-pay 480) (* 15/100 gross-pay))
    (else (* 28/100 gross-pay))))

(= 0 (tax 10))
(= 0 (tax 240))
(= 45 (tax 300))
(= 72 (tax 480))
(= 140 (tax 500))

(define (gross-pay hours)
  (* 12 hours))

(define (netpay hours)
  (- (gross-pay hours) (tax (gross-pay hours))))

(= 12 (netpay 1))
(= 408 (netpay 40))

;; Exercise 4.4.3

(define (pay-back-0-500 charge)
  (* 0.0025 charge))

(define (pay-back-500-1500 charge)
  (+ (pay-back-0-500 500)
     (* 0.005 (- charge 500))))

(define (pay-back-1500-2500 charge)
  (+ (pay-back-500-1500 1500)
     (* 0.0075 (- charge 1500))))

(define (pay-back-2500+ charge)
  (+ (pay-back-1500-2500 2500)
     (* 0.01 (- charge 2500))))

(define (pay-back charge)
  (cond
    [(<= charge 500) (pay-back-0-500 charge)]
    [(<= charge 1500) (pay-back-500-1500 charge)]
    [(<= charge 2500) (pay-back-1500-2500 charge)]
    [else (pay-back-2500+ charge)]))

(= 1 (pay-back 400))
(= 5.75 (pay-back 1400))
(= 10.0 (pay-back 2000))
(= 14.75 (pay-back 2600))

;; Exercise 4.4.4

(define (how-many a b c)
  (let [(result (- (sqr b) (* 4 a c)))]
    (cond
      [(> result 0) 2]
      [(= result 0) 1]
      [else 0])))

(= 2 (how-many 1 0 -1))
(= 1 (how-many 2 4 2))
(= 1 (how-many 1 2 1))
(= 2 (how-many 2 4 1))
(= 0 (how-many 2 4 3))
(= 2 (how-many 1 0 -1))
(= 1 (how-many 2 4 2))