#lang racket
(require test-engine/racket-tests)
(require lang/posn)
(require htdp/draw)

;; Exercise 25.1.1
(define WIDTH 100)
(define HEIGHT 100)
(define DELAY 1)

;; model a ball on a table
(define-struct ball (x y delta-x delta-y))

(define (draw-and-clear a-ball)
  (and
    (draw-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)
    (sleep-for-a-while DELAY)
    (clear-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)))

(define (move-ball a-ball)
  (make-ball (+ (ball-x a-ball) (ball-delta-x a-ball))
             (+ (ball-y a-ball) (ball-delta-y a-ball))
             (ball-delta-x a-ball)
             (ball-delta-y a-ball)))

(define the-ball (make-ball 10 20 5 17))

(define (out-of-bounds? a-ball)
  (not
    (and (<= 0 (ball-x a-ball) WIDTH)
         (<= 0 (ball-y a-ball) HEIGHT))))

(define (move-until-out a-ball)
  (cond
    [(out-of-bounds? a-ball) true]
    [else (and (draw-and-clear a-ball)
               (move-until-out (move-ball a-ball)))]))

(start WIDTH HEIGHT)
;;(move-until-out (make-ball 10 20 0 0))
(move-until-out the-ball)
(stop)
