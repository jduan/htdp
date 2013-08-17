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
