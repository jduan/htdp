#lang racket
(require test-engine/racket-tests)

;; Exercise 27.1.1
(require lang/posn)
(require htdp/draw)

(define (sierpinski a b c)
  (define (midpoint pa pb)
    (make-posn (/ (+ (posn-x pa) (posn-x pb)) 2)
               (/ (+ (posn-y pa) (posn-y pb)) 2)))
  (let ([ab (midpoint a b)]
        [ac (midpoint a c)]
        [bc (midpoint b c)])
    (if (too-small? a b c)
      true
      (and (draw-triangle a b c)
           (sierpinski a ab ac)
           (sierpinski ab b bc)
           (sierpinski ac bc c)))))

(define (too-small? a b c)
  (<= (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
               (sqr (- (posn-y a) (posn-y b)))))
      20))

(define (draw-triangle a b c)
  (draw-solid-line a b 'red)
  (draw-solid-line a c 'red)
  (draw-solid-line b c 'red))

(start 500 500)
(sierpinski (make-posn 200 0) (make-posn 0 400) (make-posn 400 400))

(test)
