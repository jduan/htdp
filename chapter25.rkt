#lang racket
(require test-engine/racket-tests)
(require lang/posn)
(require htdp/draw)

;; Exercise 25.1.1
(define WIDTH 100)
(define HEIGHT 100)
(define DELAY 1)

;; model a ball on a table
(define-struct ball (x y delta-x delta-y))

(define (draw-and-clear a-ball)
  (and
    (draw-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)
    (sleep-for-a-while DELAY)
    (clear-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)))

(define (move-ball a-ball)
  (make-ball (+ (ball-x a-ball) (ball-delta-x a-ball))
             (+ (ball-y a-ball) (ball-delta-y a-ball))
             (ball-delta-x a-ball)
             (ball-delta-y a-ball)))

(define the-ball (make-ball 10 20 5 17))

(define (out-of-bounds? a-ball)
  (not
    (and (<= 0 (ball-x a-ball) WIDTH)
         (<= 0 (ball-y a-ball) HEIGHT))))

(define (move-until-out a-ball)
  (cond
    [(out-of-bounds? a-ball) true]
    [else (and (draw-and-clear a-ball)
               (move-until-out (move-ball a-ball)))]))

;(start WIDTH HEIGHT)
;;;(move-until-out (make-ball 10 20 0 0))
;(move-until-out the-ball)
;(stop)

;; Exercise 25.1.2
(define (move-all-balls-until-out list-of-balls)
  (cond
    [(andmap out-of-bounds? list-of-balls) true]
    [else
     (and (draw-and-clear-all-balls list-of-balls)
          (move-all-balls-until-out (map move-ball list-of-balls)))]))

(define (draw-and-clear-all-balls list-of-balls)
  (and
    (andmap (lambda (a-ball) (draw-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)) list-of-balls)
    (sleep-for-a-while DELAY)
    (andmap (lambda (a-ball) (clear-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)) list-of-balls)))

(start WIDTH HEIGHT)
(move-all-balls-until-out
  (list (make-ball 10 20 5 5)
        (make-ball 30 40 -5 -3)))
(stop)

;; quicksort
(define (qsort numbers)
  (cond
    [(empty? numbers) empty]
    [else
     (let [(pivot (first numbers))]
       (append (qsort (smaller-numbers (rest numbers) pivot))
               (list pivot)
               (qsort (bigger-numbers (rest numbers) pivot))))]))

(define (smaller-numbers numbers pivot)
  (cond
    [(empty? numbers) empty]
    [else (filter (lambda (n) (<= n pivot)) numbers)]))

(define (bigger-numbers numbers pivot)
  (cond
    [(empty? numbers) empty]
    [(filter (lambda (n) (> n pivot)) numbers)]))

(qsort '(3 1 5 3 2))

;; Exercise 25.2.2
(define (qsort numbers)
  (cond
    [(empty? numbers) empty]
    [(empty? (rest numbers)) numbers]
    [else
     (let [(pivot (first numbers))]
       (append (qsort (smaller-numbers (rest numbers) pivot))
               (list pivot)
               (qsort (bigger-numbers (rest numbers) pivot))))]))

(define (smaller-numbers numbers pivot)
  (cond
    [(empty? numbers) empty]
    [else (filter (lambda (n) (<= n pivot)) numbers)]))

(define (bigger-numbers numbers pivot)
  (cond
    [(empty? numbers) empty]
    [(filter (lambda (n) (> n pivot)) numbers)]))

(qsort '(3 1 5 3 2))

;; Exercise 25.2.3
(define (insert-to-sorted-list n alon sort-f)
  (if (empty? alon)
    (cons n empty)
    (if (sort-f n (first alon))
      (cons n alon)
      (cons (first alon) (insert-to-sorted-list n (rest alon) sort-f)))))

;; Sort a list of numbers in ascending order
(define (mysort alon sort-f)
  (if (empty? alon)
    empty
    (insert-to-sorted-list
      (first alon)
      (mysort (rest alon) sort-f)
      sort-f)))

(define (sort-n alon)
  (mysort alon <))

(define (qsort2 numbers)
  (cond
    [(< (length numbers) 4) (sort-n numbers)]
    [else
     (let [(pivot (first numbers))]
       (append (qsort2 (smaller-numbers (rest numbers) pivot))
               (list pivot)
               (qsort2 (bigger-numbers (rest numbers) pivot))))]))

(define (smaller-numbers numbers pivot)
  (cond
    [(empty? numbers) empty]
    [else (filter (lambda (n) (<= n pivot)) numbers)]))

(define (bigger-numbers numbers pivot)
  (cond
    [(empty? numbers) empty]
    [(filter (lambda (n) (> n pivot)) numbers)]))

(qsort2 '(3 1 5 3 2))

;; Exercise 25.2.6
(define (general-qsort pred? lst)
  (cond
    [(empty? lst) lst]
    [else
     (let ([pivot (first lst)])
       (append (general-qsort pred? (left-half (rest lst) pivot pred?))
               (list pivot)
               (general-qsort pred? (right-half (rest lst) pivot pred?))))]))

(define (left-half lst pivot pred?)
  (filter (lambda (e) (pred? e pivot)) lst))

(define (right-half lst pivot pred?)
  (filter (lambda (e) (not (pred? e pivot))) lst))

(general-qsort < '(3 1 5 3 2 ))
