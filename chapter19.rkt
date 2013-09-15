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
