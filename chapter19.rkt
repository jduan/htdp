;; Filter a list of numbers based on a comparison operator and a threshold.
(define (filter1 op lon t)
  (cond
    [(empty? lon) empty]
    [(op (first lon) t) (cons (first lon) (filter1 op (rest lon) t))]
    [else (filter1 op (rest lon) t)]))

(define (below1 lon t)
  (filter1 < lon t))

(define (above1 lon t)
  (filter1 > lon t))

(equal? (below1 '(6 4) 5) '(4))
(equal? (above1 '(8 6 4) 5) '(8 6))

(define (squared>? x c)
  (> (* x x) c))

(equal? (filter1 squared>? '(1 2 3 4 5) 10) '(4 5))

(define (filter pred? lon)
  (cond
    [(empty? lon) empty]
    [(pred? (first lon)) (cons (first lon) (filter pred? (rest lon)))]
    [else (filter pred? (rest lon))]))

(define (below2 lon t)
  (local ((define (pred? x) (< x t)))
         (filter pred? lon)))

(equal? (below2 '(6 4) 5) '(4))

(define (above2 lon t)
  (local ((define (pred? x) (> x t)))
         (filter pred? lon)))

(equal? (above2 '(8 6 4) 5) '(8 6))
(equal? (above2 (list 1 2 3 4 5) 3)
        (list 4 5))

;; Exercise 19.1.5
;; Find a single number from a list of numbers based on a criteria.
(define (find op lon)
  (cond
    [(empty? (rest lon)) (first lon)]
    [(op (first lon) (find op (rest lon))) (first lon)]
    [else (find op (rest lon))]))

(define (mini1 lon)
  (find < lon))

(define (maxi1 lon)
  (find > lon))

(equal? (mini1 (list 3 7 6 2 9 8)) 2)
(equal? (mini1 (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(equal? (mini1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 1)
(equal? (maxi1 (list 3 7 6 2 9 8)) 9)
(equal? (maxi1 (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 20)
(equal? (maxi1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 20)

(define (find2 op lon)
  (cond
    [(empty? (rest lon)) (first lon)]
    [else
     (local ((define rest-result (find op (rest lon))))
            (cond
              [(op (first lon) rest-result) (first lon)]
              [else (find2 op (rest lon))]))]))

(define (mini2 lon)
  (find2 < lon))

(define (maxi2 lon)
  (find2 > lon))

(equal? (mini2 (list 3 7 6 2 9 8)) 2)
(equal? (mini2 (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(equal? (mini2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 1)
(equal? (maxi2 (list 3 7 6 2 9 8)) 9)
(equal? (maxi2 (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 20)
(equal? (maxi2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 20)

;; Exercise 19.1.6
;; sort : list-of-numbers  ->  list-of-numbers
;; to construct a list with all items from alon sorted by the op
(define (sort alon op)
  (local [(define (sort alon)
            (cond
              [(empty? alon) empty]
              [else (insert (first alon) (sort (rest alon)))]))
          (define (insert an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(op an (first alon)) (cons an alon)]
                      [else (cons (first alon) (insert an (rest alon)))])]))]
         (sort alon)))

(equal? (sort '(2 3 1 5 4) <) '(1 2 3 4 5))
(equal? (sort '(2 3 1 5 4) >) '(5 4 3 2 1))

;; Exercise 19.2.2
(define-struct ir (name price))

(define inventory-records
  (list (make-ir 'book 25)
        (make-ir 'video 5)
        (make-ir 'audio 7)
        (make-ir 'ce 100)))

(sort inventory-records (lambda (r1 r2) (< (ir-price r1) (ir-price r2))))
(sort inventory-records (lambda (r1 r2) (> (ir-price r1) (ir-price r2))))

;; Exercise 19.2.3
;; Develop the function lefts, which consumes a list of (pair X Y) and produces
;; a corresponding list of X's
(define-struct pair (left right))

(define (lefts list-of-pairs)
  (cond
    [(empty? list-of-pairs) empty]
    [else (cons (pair-left (first list-of-pairs)) (lefts (rest list-of-pairs)))]))

(define list-of-pairs-of-numbers
  (list (make-pair 1 2)
        (make-pair 3 4)
        (make-pair 5 6)
        (make-pair 7 8)))
(equal? (lefts list-of-pairs-of-numbers) '(1 3 5 7))
