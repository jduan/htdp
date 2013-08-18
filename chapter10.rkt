;; Exercise 10.1.1
;; calculate the total wage of someone who worked for h hours
(define (wage h)
  (* 14 h))
;; calculate the wages for a given list of hours
(define (hours->wages a-list-of-hours)
  (cond
    [(empty? a-list-of-hours) empty]
    [else (cons (wage (first a-list-of-hours))
                (hours->wages (rest a-list-of-hours)))]))
(equal? '(140 280 420) (hours->wages '(10 20 30)))
(equal? '(28 196 2954) (hours->wages '(2 14 211)))

;; Exercise 10.1.2
;; calculate the total wage of someone who worked for h hours
(define (wage h)
  (if (> h 100)
    (error 'wage "too many hours")
    (* 14 h)))

;; calculate the wages for a given list of hours
(define (hours->wages a-list-of-hours)
  (cond
    [(empty? a-list-of-hours) empty]
    [else (cons (wage (first a-list-of-hours))
                (hours->wages (rest a-list-of-hours)))]))
(equal? '(140 280 420) (hours->wages '(10 20 30)))
(equal? '(28 196 2954) (hours->wages '(2 14 211)))

(define (f->c degree)
  (* 5/9 (- degree 32)))

(define (convertFC lof)
  (cond
    [(empty? lof) empty]
    [else (cons (f->c (first lof))
                (convertFC (rest lof)))]))

(equal? '(0 100 30) (convertFC '(32 212 86)))

;; Exercise 10.1.4
(define (convert-euro lon rate)
  (cond
    [(empty? lon) empty]
    [else (cons (* rate (first lon))
                (convert-euro (rest lon) rate))]))

(equal? '(0.88) (convert-euro '(0.50) 1.76))

;; Exercise 10.1.5
(define (eliminate-exp ua lotp)
  (cond
    [(empty? lotp) empty]
    [else (cond
            [(<= (first lotp) ua)
             (cons (first lotp)
                   (eliminate-exp ua (rest lotp)))]
            [else
             (eliminate-exp ua (rest lotp))])]))

(equal? '(0.95 1.0) (eliminate-exp 1.0 '(2.95 0.95 1.0 5)))

;; Exercise 10.1.6
(define (substitute new old los)
  (cond
    [(empty? los) empty]
    [else (if (symbol=? old (first los))
            (cons new (substitute new old (rest los)))
            (cons (first los) (substitute new old (rest los))))]))

(equal? '(robot Barbie dress) (substitute 'Barbie 'doll '(robot doll dress)))

;; Exercise 10.1.7
(define (recall ty lon)
  (cond
    [(empty? lon) empty]
    [else (if (symbol=? ty (first lon))
            (recall ty (rest lon))
            (cons (first lon) (recall ty (rest lon))))]))

(equal? '(doll dress) (recall 'robot '(robot doll dress)))

;; Exercise 10.1.8
(define (controller amount)
  (let* [(dollars (quotient amount 100))
         (cents (remainder amount 100))
         (d-sym (if (> dollars 1) 'dollars 'dollar))
         (c-sym (if (> cents 1) 'cents 'cent))]
    (list dollars d-sym 'and cents c-sym)))

(equal? '(1 dollar and 3 cents) (controller 103))
(equal? '(32 dollars and 78 cents) (controller 3278))


;; structure: ir is a struct of a name and a price
(define-struct ir (name price))

(define (sum an-inv)
  (cond
    [(empty? an-inv) 0]
    [else (+ (ir-price (first an-inv))
             (sum (rest an-inv)))]))
