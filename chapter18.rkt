;; Exercise 18.1.1
(local [(define (f x) (+ x 5))
        (define (g alon)
          (cond
            [(empty? alon) empty]
            [else (cons (f (first alon)) (g (rest alon)))]))]
       (g (list 1 2 3)))

(local [(define (odd an)
          (cond
            [(zero? an) false]
            [else (even (sub1 an))]))
        (define (even an)
          (cond
            [(zero? an) true]
            [else (odd (sub1 an))]))]
       (even 12))

;; Exercise 18.1.2
(local [(define (f x) (+ (* x x) (* 3 x) 15))
        (define x 100)
        (define f@100 (f x))]
       f@100 x )

(local ((define (f x) (+ (* x x) (* 3 x) 14))
          (define x 100)
          (define g (f x)))
       g)

;; Exercise 18.1.3
(define A-CONSTANT
  (not
    (local [(define (odd an)
              (cond
                [(= an 0) false]
                [else (even (- an 1))]))
            (define (even an)
              (cond
                [(= an 0) true]
                [else (odd (- an 1))]))]
           (even 3))))

(+ (local [(define (f x) (+ (* x x) (* 3 x) 15))
           (define x 100)
           (define f@100 (f x))]
          f@100)
   1000)

(local ((define CONST 100)
          (define (f x) (+ x CONST)))
       (define (g x y z) (f (+ x (* y z)))))

;; Exercise 18.1.4
;; Impossible. Locals are locals.

;; Exercise 18.1.5
(equal? 30 (local ((define (x y) (* 3 y)))
                  (* (x 2) 5)))

(equal? -18 (local ((define (f c) (+ (* 9/5 c) 32)))
                   (- (f 0) (f 10))))

(equal? false (local [(define (odd? n)
                        (cond
                          [(zero? n) false]
                          [else (even? (sub1 n))]))
                      (define (even? n)
                        (cond
                          [(zero? n) true]
                          [else (odd? (sub1 n))]))]
                     (even? 1)))

(equal? 588 (+ (local ((define (f x) (g (+ x 1) 22))
                         (define (g x y) (+ x y)))
                      (f 10))
               555))

(define (h n)
  (cond
    [(= n 0) empty]
    [else (local ((define r (* n n)))
                 (cons r (h (- n 1))))]))
(equal? '(4 1) (h 2))
;; Determine the largest number of a list of numbers
(define (maxi nums)
  (cond
    [(empty? (rest nums)) (first nums)]
    [else (local ((define rest-max (maxi (rest nums))))
                (if (> (first nums) rest-max)
                  (first nums)
                  rest-max))]))

(equal? 20 (maxi (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(equal? 30 (maxi (list 1 2 3 4 5 6 7 8 9 10 11 30 13 14 15 16 17 18 19 20)))

;; Exercise 18.1.13
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
(define Django (make-child empty Eva 'Django 1990 'grey))

(define Hal (make-child Gustav empty 'Gustav 1988 'hazel))

;; to-blue-eyed-ancestor : ftn  ->  path or false
;; to compute the path from a-ftn tree to a blue-eyed ancestor
;; A path is a list of 'father and 'mother. The empty path indicates that a-ftn
;; has 'blue in the eyes field'
(define (to-blue-eyed-ancestor a-ftn)
  (local [(define ancestor-father-side
            (cond
              [(child? (child-father a-ftn)) (to-blue-eyed-ancestor (child-father a-ftn))]
              [else false]))
          (define ancestor-mother-side
            (cond
              [(child? (child-mother a-ftn)) (to-blue-eyed-ancestor (child-mother a-ftn))]
              [else false]))]
         (cond
           [(symbol=? 'blue (child-eyes a-ftn)) empty]
           [(list? ancestor-father-side) (cons 'father ancestor-father-side)]
           [(list? ancestor-mother-side) (cons 'mother ancestor-mother-side)]
           [else false])))

(equal? empty (to-blue-eyed-ancestor Eva))
(equal? false (to-blue-eyed-ancestor Fred))
(equal? '(mother) (to-blue-eyed-ancestor Gustav))
(equal? false (to-blue-eyed-ancestor Dave))
(equal? false (to-blue-eyed-ancestor Adam))
(equal? false (to-blue-eyed-ancestor Bettina))
(equal? false (to-blue-eyed-ancestor Carl))
(equal? '(father mother) (to-blue-eyed-ancestor Hal))

;; mult10 : list-of-digits  ->  list-of-numbers
;; to create a list of numbers by multiplying each digit on alod
;; by (expt 10 p) where p is the number of digits that follow
(define (multi10 digits)
  (local ((define (multi10-helper digits total)
            (cond
              [(empty? digits) total]
              [else (multi10-helper (rest digits) (+ (first digits) (* 10 total)))])))
         (multi10-helper digits 0)))

(equal? 123 (multi10 '(1 2 3)))
(equal? 32 (multi10 '(3 2)))
