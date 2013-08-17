(require lang/posn)
(require htdp/draw)
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

;; Exercise 6.3.2
(symbol=? 'ThePhantomMenace
          (movie-title (make-movie 'ThePhantomMenace 'Lucas)))
(symbol=? 'Lucas
          (movie-producer (make-movie 'TheEmpireStrikesBack 'Lucas)))

(define-struct star (last first instrument sales))

(define (increment-sales a-star)
  (make-star (star-last a-star)
             (star-first a-star)
             (star-instrument a-star)
             (+ (star-sales a-star) 20000)))

(= 32200 (star-sales (increment-sales (make-star 'Abba 'John 'vocals 12200))))

;; Exercise 6.3.3
(define-struct jet-fighters (designation accelation top-speed range))

(define (within-range jet range)
  (< range (jet-fighters-range jet)))

(within-range (make-jet-fighters 'f22 'fast 2000 5000) 4000)

(define (reduce-range jet)
  (make-jet-fighters
    (jet-fighters-designation jet)
    (jet-fighters-accelation jet)
    (jet-fighters-top-speed jet)
    (* 8/10 (jet-fighters-range jet))))

(= 4000 (jet-fighters-range (reduce-range (make-jet-fighters 'f22 'fast 2000 5000))))

;; Exercise 6.4.2
;; number number number
(define-struct time (hours minutes seconds))

;; Exercise 6.5.2
(define (time->seconds time)
  (+ (* 60 60 (time-hours time))
     (* 60 (time-minutes time))
     (time-seconds time)))

(= 45002 (time->seconds (make-time 12 30 2)))

;; Exercise 6.6.1
;; center is a posn structure
;; radius is a number
;; color is a symbol
(define-struct circle (center radius color))

;; template
;; (define (fun-for-circle circle)
;;   (posn-x (circle-center circle))
;;   (posn-y (circle-center circle))
;;   (circle-radius circle)
;;   (circle-color circle)

;; Exercise 6.6.2
;; draw-a-circle: circle -> true
;; draws the disk on the screen
(define (draw-a-circle circle)
  (draw-circle (circle-center circle)
               (circle-radius circle)
               (circle-color circle)))
(start 300 300)
(draw-a-circle
  (make-circle (make-posn 100 100)
               100
               'red))

;; Exercise 6.6.3
;; distance: posn1, posn2 -> float
;; returns the distance between two points
(define (distance posn1 posn2)
  (sqrt (+ (sqr (- (posn-x posn1) (posn-x posn2)))
           (sqr (- (posn-y posn1) (posn-y posn2))))))
(= (sqrt 2) (distance (make-posn 1 1) (make-posn 2 2)))
;; in-circle? : circle, posn -> bool
;; check if a pixel (posn) is in a circle or not
(define (in-circle? circle posn)
  (<= (distance (circle-center circle) posn)
      (circle-radius circle)))
(in-circle? (make-circle (make-posn 6 2)
                         1
                         'red)
            (make-posn 6 1.5))
(not (in-circle? (make-circle (make-posn 6 2)
                              1
                              'red)
                 (make-posn 8 6)))
(in-circle? (make-circle (make-posn 6 5) 1 'blue) (make-posn 6 5))
(in-circle? (make-circle (make-posn 6 5) 1 'green) (make-posn 5.5 5))
(not (in-circle? (make-circle (make-posn 6 5) 1 'yellow) (make-posn 1 5)))

;; Exercise 6.6.4
;; translate-circle : circle, delta -> circle
;; translate a circle to another circle which is delta pixel to the right of the
;; original one
(define (translate-circle circle delta)
  (make-circle
    (make-posn (+ delta (posn-x (circle-center circle)))
               (posn-y (circle-center circle)))
    (circle-radius circle)
    (circle-color circle)))
(= 10 (posn-x (circle-center (translate-circle (make-circle (make-posn 0 0) 5 'blue) 10))))

;; Exercise 6.6.5
(define (clear-a-circle a-circle)
  (clear-circle (circle-center a-circle)
                (circle-radius a-circle)))
(clear-a-circle (make-circle (make-posn 100 100) 100 'red))

;; Exercise 6.6.6
(define (draw-and-clear-circle a-circle)
  (draw-a-circle a-circle)
  (sleep-for-a-while 2)
  (clear-a-circle a-circle))
(draw-and-clear-circle (make-circle (make-posn 100 100) 100 'red))

(define (move-a-circle delta a-circle)
  (cond
    [(draw-and-clear-circle a-circle) (translate-circle a-circle delta)]
    [else a-circle]))
(move-a-circle 50 (make-circle (make-posn 100 100) 100 'red))

(define (circle-animation a-circle delta)
  (draw-and-clear-circle
    (move-a-circle delta
                   (move-a-circle delta
                                  (move-a-circle delta a-circle)))))
(circle-animation (make-circle (make-posn 100 100) 50 'red) 50)

;; Exercise 6.6.7
(define-struct rectangle (nw-corner width height color))

;; Exercise 6.6.8
;; draw a rectangle and fill it with color
(define (draw-a-rectangle rect)
  (draw-solid-rect (rectangle-nw-corner rect)
                   (rectangle-width rect)
                   (rectangle-height rect)
                   (rectangle-color rect)))
(draw-a-rectangle (make-rectangle (make-posn 100 100) 50 50 'red))
