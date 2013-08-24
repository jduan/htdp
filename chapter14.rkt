(define-struct child (father mother name date eyes))

;; oldest generation
(define Carl (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

;; middle generation
(define Adam (make-child Carl Bettina 'Adam 1950 'yellow))
(define Dave (make-child Carl Bettina 'Dave 1955 'black))
(define Eva (make-child Carl Bettina 'Eva 1965 'blue))
(define Fred (make-child empty empty 'Fred 1966 'pink))

;; youngest generation
(define Gustav (make-child Eva Fred 'Gustav 1988 'brown))

(define (blue-eyed-ancestor? child)
  (cond
    [(empty? child) false]
    [else (or (symbol=? 'blue (child-eyes child))
              (blue-eyed-ancestor? (child-father child))
              (blue-eyed-ancestor? (child-mother child)))]))

(equal? false (blue-eyed-ancestor? Carl))
(equal? true (blue-eyed-ancestor? Gustav))
