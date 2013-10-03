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

;(start WIDTH HEIGHT)
;;;(move-until-out (make-ball 10 20 0 0))
;(move-until-out the-ball)
;(stop)

;; Exercise 25.1.2
(define (move-all-balls-until-out list-of-balls)
  (cond
    [(andmap out-of-bounds? list-of-balls) true]
    [else
     (and (draw-and-clear-all-balls list-of-balls)
          (move-all-balls-until-out (map move-ball list-of-balls)))]))

(define (draw-and-clear-all-balls list-of-balls)
  (and
    (andmap (lambda (a-ball) (draw-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)) list-of-balls)
    (sleep-for-a-while DELAY)
    (andmap (lambda (a-ball) (clear-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)) list-of-balls)))

(start WIDTH HEIGHT)
(move-all-balls-until-out
  (list (make-ball 10 20 5 5)
        (make-ball 30 40 -5 -3)))
(stop)

;; quicksort
(define (qsort numbers)
  (cond
    [(empty? numbers) empty]
    [else
     (let [(pivot (first numbers))]
     (append (qsort (smaller-numbers numbers pivot))
             (first numbers)
             (qsort (bigger-numbers numbers ()))))]))

(define (smaller-numbers numbers pivot)
  (filter (lambda (n) (<= n pivot)) numbers))

(define (bigger-numbers numbers pivot)
  (filter (lambda (n) (> n pivot)) numbers))

(qsort '(3 1 5 3 2))
