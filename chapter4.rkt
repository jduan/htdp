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
