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
(define Gustav (make-child Fred Eva 'Gustav 1988 'brown))

(define (blue-eyed-ancestor? child)
  (cond
    [(empty? child) false]
    [else (or (symbol=? 'blue (child-eyes child))
              (blue-eyed-ancestor? (child-father child))
              (blue-eyed-ancestor? (child-mother child)))]))

(equal? false (blue-eyed-ancestor? Carl))
(equal? true (blue-eyed-ancestor? Gustav))

;; Exercise 14.1.3
(define (count-persons node)
  (cond
    [(empty? node) 0]
    [else (+ 1
             (count-persons (child-father node))
             (count-persons (child-mother node)))]))

(equal? 0 (count-persons empty))
(equal? 1 (count-persons Carl))
(equal? 1 (count-persons Fred))
(equal? 3 (count-persons Adam))
(equal? 5 (count-persons Gustav))

;; Exercise 14.1.4
(define (add-all-ages node current-year)
  (cond
    [(empty? node) 0]
    [else (+ (add-all-ages (child-father node) current-year)
             (add-all-ages (child-mother node) current-year)
             (- current-year (child-date node)))]))

;; Doesn't handle empty node!
(define (average-age node current-year)
  (/ (add-all-ages node current-year)
     (count-persons node)))

(equal? 75 (average-age Carl 2001))
(equal? 75 (average-age Bettina 2001))
(equal? 67 (average-age Adam 2001))
(equal? (+ 65 1/3) (average-age Dave 2001))
(equal? 62 (average-age Eva 2001))
(equal? 35 (average-age Fred 2001))
(equal? (+ 46 4/5) (average-age Gustav 2001))

;; Exercise 14.1.5
(define (eye-colors node)
  (cond
    [(empty? node) empty]
    [else (cons (child-eyes node)
                (append (eye-colors (child-father node))
                        (eye-colors (child-mother node))))]))

(equal? empty (eye-colors empty))
(equal? (list 'green) (eye-colors Carl ))
(equal? (list 'green) (eye-colors Bettina ))
(equal? (list 'yellow 'green 'green) (eye-colors Adam ))
(equal? (list 'black 'green 'green) (eye-colors Dave ))
(equal? (list 'blue 'green 'green) (eye-colors Eva ))
(equal? (list 'pink) (eye-colors Fred ))
(equal? (list 'brown 'pink 'blue 'green 'green) (eye-colors Gustav ))
