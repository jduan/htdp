(require lang/posn)
;; Exercise 6.1.1

(define (distance-to-0 a-posn)
  (sqrt
    (+ (sqr (posn-x a-posn))
       (sqr (posn-y a-posn)))))

(= 5 (distance-to-0 (make-posn 3 4)))
(= 10 (distance-to-0 (make-posn (* 2 3) (* 2 4))))
(= 13 (distance-to-0 (make-posn 12 (- 6 1))))

;; Exercise 6.2.2
(define WIDTH 50)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

;; the positions of the bulbs
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

;; draw the light with the red bulb turned on
(start WIDTH HEIGHT)
(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)

(define (clear-bulb color)
  (cond
    [(symbol=? color 'red)
     (and (clear-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
          (draw-circle (make-posn X-BULBS Y-RED) BULB-RADIUS 'red))]
    [(symbol=? color 'yellow)
     (and (clear-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
          (draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow))]
    [(symbol=? color 'green)
     (and (clear-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)
          (draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green))]
    [else "wrong color"]))

;; Exercise 6.2.3
(define (draw-bulb color)
  (cond
    [(symbol=? color 'red)
     (draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red) ]
    [(symbol=? color 'yellow)
     (draw-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow) ]
    [(symbol=? color 'green)
     (draw-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green) ]
    [else "wrong color"]))

;; Exercise 6.2.4
(define (switch from to)
  (and (clear-bulb from)
       (draw-bulb to)))

;; Exercise 6.2.5
(define (next current-color)
  (cond
    [(and (symbol=? current-color 'red) (switch 'red 'green))
     'green]
    [(and (symbol=? current-color 'yellow) (switch 'yellow 'red))
     'red]
    [(and (symbol=? current-color 'green) (switch 'green 'yellow))
     'yellow]))
(define WIDTH 50)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

;; the positions of the bulbs
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

;; draw the light with the red bulb turned on
(start WIDTH HEIGHT)
(draw-bulb 'red)

;; Exercise 6.3.1
(define-struct movie (title producer))
(define jobs (make-movie 'Jobs 'Kutcher))
(symbol=? 'Jobs (movie-title jobs))
(symbol=? 'Kutcher (movie-producer jobs))
