;; compute the distance of a pixel to the origin
(define (distance-to-0 a-pixel)
  (cond
    [(number? a-pixel) a-pixel]
    [(posn? a-pixel) (sqrt
                       (+ (sqr (posn-x a-pixel))
                          (sqr (posn-y a-pixel))))]))

(define-struct square (nw length))
(define-struct circle (center radius))

;; Exercise 7.1.2
;; perimeter : shape -> number
;; compute the perimeter of a shape
(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* 4 (square-length a-shape))]
    [(circle? a-shape) (* 2 pi (circle-radius a-shape))]))

(= (perimeter (make-square (make-posn 2 2) 3)) 12)
(= (* 40 pi) (perimeter (make-circle (make-posn 30 30) 20)))
