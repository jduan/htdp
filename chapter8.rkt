(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 0.001))

;; Exercise 8.1.1
(cons 'Mercury
      (cons 'Venus
            (cons 'Earth
                  (cons 'Mars
                        (cons 'Jupiter
                              (cons 'Saturn
                                    (cons 'Uranus
                                          (cons 'Neptune empty))))))))

(cons 'steak
      (cons 'pommes-frites
            (cons 'beans
                  (cons 'bread
                        (cons 'water
                              (cons 'juice
                                    (cons 'brie-cheese
                                          (cons 'ice-cream empty))))))))

(cons 'red
      (cons 'oragen
            (cons 'yellow
                  (cons 'green
                        (cons 'blue
                              (cons 'indigo
                                    (cons 'violet
                                          empty)))))))

(cons 'RobbyRound
      (cons 3
            (cons true
                  empty)))

;; Exercise 9.1.3
(define (add-up-3 a-list-of-3-numbers)
  (+ (first a-list-of-3-numbers)
     (first (rest a-list-of-3-numbers))
     (first (rest (rest a-list-of-3-numbers)))))
(= 6 (add-up-3 '(1 2 3)))
(= 126 (add-up-3 '(7 41 78)))

(define (distance-to-0-for-2 x y)
  (sqrt (+ (sqr x) (sqr y))))
(= 5 (distance-to-0-for-2 3 4))

(define (distance-to-0-for-3 lst)
  (distance-to-0-for-2 (first lst)
                       (distance-to-0-for-2 (first (rest lst))
                                            (first (rest (rest lst))))))
(close-enough? (distance-to-0-for-3 '(3 4 12)) 13)
(close-enough? (distance-to-0-for-3 '(1 1 1)) (sqrt 3))
(close-enough? (distance-to-0-for-3 '(-1 2 -1)) (sqrt 6))
