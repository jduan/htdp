#lang racket

;; Exercise 5.1.1

(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

(symbol=? (reply 'HowAreYou?) 'Fine)
(symbol=? (reply 'GoodMorning) 'Hi)
(symbol=? (reply 'GoodAfternoon) 'INeedANap)
(symbol=? (reply 'GoodEvening) 'BoyAmITired)

;; Exercise 5.1.2

(define (check-guess guess target)
  (cond
    [(< guess target) 'TooSmall]
    [(= guess target) 'Perfect]
    [(> guess target) 'TooLarge]))

(symbol=? 'Perfect (check-guess 1 1))
(symbol=? 'TooSmall (check-guess 1 2))
(symbol=? 'TooLarge (check-guess 1 0))

;; Exercise 5.1.3

(define (form-number ones tens hundreds)
  (+ ones (* 10 tens) (* 100 hundreds)))

(define (check-guess3 ones tens hundreds target)
  (check-guess (form-number ones tens hundreds) target))

(symbol=? 'TooSmall (check-guess3 1 2 3 500))
(symbol=? 'TooLarge (check-guess3 1 2 3 100))
(symbol=? 'Perfect (check-guess3 1 2 3 321))

;; Exercise 5.1.4
(define (what-kind a b c)
  (let [(discriminant (- (sqr b) (* 4 a c)))]
    (cond
      [(zero? a) 'degenerate]
      [(> discriminant 0) 'two]
      [(= discriminant 0) 'one]
      [(< discriminant 0) 'none])))

(symbol=? 'two (what-kind ))
(symbol=? 'two (what-kind 1 0 -1))
(symbol=? 'one (what-kind 2 4 2))
(symbol=? 'one (what-kind 1 2 1))
(symbol=? 'two (what-kind 2 4 1))
(symbol=? 'none (what-kind 2 4 3))
(symbol=? 'two (what-kind 1 0 -1))
(symbol=? 'one (what-kind 2 4 2))

;; Exercise 5.1.5

(define (check-color target1 target2 guess1 guess2)
  (cond
    [(and (symbol=? target1 guess1) (symbol=? target2 guess2)) 'Perfect]
    [(or (symbol=? target1 guess1) (symbol=? target2 guess2)) 'OneColorAtCorrectPosition]
    [(or (symbol=? target1 guess2) (symbol=? target2 guess1)) 'OneColorOccurs]
    [else 'NothingCorrect]))

(symbol=? (check-color 'red 'green 'red 'green) 'Perfect)
(symbol=? (check-color 'red 'green 'red 'purple) 'OneColorAtCorrectPosition)
(symbol=? (check-color 'red 'green 'purple 'red) 'OneColorOccurs)
(symbol=? (check-color 'green 'red 'red 'purple) 'OneColorOccurs)
(symbol=? (check-color 'green 'blue 'red 'purple) 'NothingCorrect)
