(define-struct parent (children name date eyes))

(define Gustav (make-parent empty 'Gustav 1988 'brown))
(define Eva (make-parent (list Gustav) 'Eva 1965 'blue))
(define Fred (make-parent (list Gustav) 'Fred 1966 'pink))
(define Adam (make-parent empty 'Adam 1950 'yellow))
(define Dave (make-parent empty 'Dave 1955 'black))
(define Carl (make-parent (list Adam Dave Eva) 'Carl 1926 'green))
(define Bettina (make-parent (list Adam Dave Eva) 'Bettina 1926 'green))

(define (blue-eyed-children? children)
  (cond
    [(empty? children) false]
    [else (or (blue-eyed-descendant? (first children))
              (blue-eyed-children? (rest children)))]))

(define (blue-eyed-descendant? parent)
  (or
    (symbol=? 'blue (parent-eyes parent))
    (blue-eyed-children? (parent-children parent))))

(equal? false (blue-eyed-descendant? Gustav))
(equal? false (blue-eyed-descendant? Fred))
(equal? true (blue-eyed-descendant? Eva))
(equal? false (blue-eyed-descendant? Dave))
(equal? false (blue-eyed-descendant? Adam))
(equal? true (blue-eyed-descendant? Bettina))
(equal? true (blue-eyed-descendant? Carl))

;; Exercise 15.1.2
(define (how-far-removed-children children)
  (let* [(how-far (map how-far-removed children))
         (filtered (filter number? how-far))]
    (cond
      [(empty? filtered) false]
      [else (+ 1 (apply min filtered))])))

(define (how-far-removed parent)
  (cond
    [(symbol=? 'blue (parent-eyes parent)) 0]
    [else (how-far-removed-children (parent-children parent))]))

(equal? false (how-far-removed Gustav))
(equal? false (how-far-removed Fred))
(equal? 0 (how-far-removed Eva))
(equal? false (how-far-removed Dave))
(equal? false (how-far-removed Adam))
(equal? 1 (how-far-removed Bettina))
(equal? 1 (how-far-removed Carl))

;; Exercise 15.1.3
(define (count-descendants-children children)
  (cond
    [(empty? children) 0]
    [else (+ (count-descendants (first children))
             (count-descendants-children (rest children)))]))
(define (count-descendants parent)
  (+ 1 (count-descendants-children (parent-children parent))))

(equal? 1 (count-descendants Gustav))
(equal? 2 (count-descendants Fred))
(equal? 2 (count-descendants Eva))
(equal? 1 (count-descendants Dave))
(equal? 1 (count-descendants Adam))
(equal? 5 (count-descendants Bettina))
(equal? 5 (count-descendants Carl))

(define (count-proper-descendants parent)
  (count-descendants-children (parent-children parent)))

(equal? 0 (count-proper-descendants Gustav))
(equal? 1 (count-proper-descendants Fred))
(equal? 1 (count-proper-descendants Eva))
(equal? 0 (count-proper-descendants Dave))
(equal? 0 (count-proper-descendants Adam))
(equal? 4 (count-proper-descendants Bettina))
(equal? 4 (count-proper-descendants Carl))
