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
