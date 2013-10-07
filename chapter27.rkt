#lang racket
(require test-engine/racket-tests)

;; Exercise 27.1.1
(require lang/posn)
(require htdp/draw)

(define (sierpinski a b c)
  (define (midpoint pa pb)
    (make-posn (/ (+ (posn-x pa) (posn-x pb)) 2)
               (/ (+ (posn-y pa) (posn-y pb)) 2)))
  (let ([ab (midpoint a b)]
        [ac (midpoint a c)]
        [bc (midpoint b c)])
    (if (too-small? a b c)
      true
      (and (draw-triangle a b c)
           (sierpinski a ab ac)
           (sierpinski ab b bc)
           (sierpinski ac bc c)))))

(define (too-small? a b c)
  (<= (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
               (sqr (- (posn-y a) (posn-y b)))))
      20))

(define (draw-triangle a b c)
  (draw-solid-line a b 'red)
  (draw-solid-line a c 'red)
  (draw-solid-line b c 'red))

;(start 500 500)
;(sierpinski (make-posn 200 0) (make-posn 0 400) (make-posn 400 400))

;; Exercise 27.2.2
(define (file->list-of-lines afile)
  (define (remove-first-line afile)
    (cond
      [(and (symbol? (first afile)) (symbol=? 'NL (first afile)) (rest afile))]
      [else (remove-first-line (rest afile))]))
  (define (first-line afile)
    (cond
      [(and (symbol? (first afile)) (symbol=? 'NL (first afile))) empty]
      [else (cons (first afile) (first-line (rest afile)))]))
  (cond
    [(empty? afile) empty]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))

(define FILE (list 'a 'b 'c 'NL 'd 'e 'NL 'f 'g 'h 'NL))
(check-expect (file->list-of-lines FILE) '((a b c) (d e) (f g h)))

;; Exercise 27.2.3
(define-struct rr (table costs) #:transparent)
(define (file->list-of-checks afile)
  (let [(list-of-lines (file->list-of-lines afile))]
    (map
      (lambda (line)
              (make-rr (first line) (rest line)))
      list-of-lines)))

(check-expect
  (equal? (file->list-of-checks
            (list 1 2.30 4.00 12.50 13.50 'NL
                  2 4.00 18.00 'NL
                  4 2.30 12.50 'NL))
          (list (make-rr 1 (list 2.30 4.00 12.50 13.50))
                (make-rr 2 (list 4.00 18.00))
                (make-rr 4 (list 2.30 12.50))))
  true)

;; Exercise 27.2.4
(define (create-matrix n lst)
  (define (first-row n lst)
    (cond
      [(zero? n) empty]
      [else (cons (first lst) (first-row (sub1 n) (rest lst)))]))
  (define (remove-first-row n lst)
    (cond
      [(zero? n) lst]
      [else (remove-first-row (sub1 n) (rest lst))]))
  (cond
    [(empty? lst) empty]
    [else (cons (first-row n lst)
                (create-matrix n (remove-first-row n lst)))]))

(check-expect (create-matrix 2 (list 1 2 3 4))
              (list (list 1 2)
                    (list 3 4)))

;; Exercise 27.3.1
(define (find-root f left right)
  (cond
    [(<= (- right left) TOLERANCE) left]
    [else
     (let [(mid (/ (+ left right) 2))]
       (cond
         [(<= (* (f left) (f mid)) 0) (find-root f left mid)]
         [else (find-root f mid right)]))]))

(define TOLERANCE 0.01)
(define (poly x)
  (* (- x 2) (- x 4)))

(check-expect (<= (- (find-root poly 3 6) 4) 0.01) true)
(check-expect (<= (- (find-root poly 1 3.5) 2) 0.01) true)
(check-expect (<= (- (find-root poly 1 2.5) 2) 0.01) true)


(test)
