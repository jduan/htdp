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

