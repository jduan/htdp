#lang racket
(require test-engine/racket-tests)

;;; Exercise 29.1.1
;(define (sum-tree tree)
;  (cond
;    [(number? tree) tree]
;    [else (+ (sum-tree (first tree)) (sum-tree (second tree)))]))

;(check-expect (sum-tree 3) 3)
;(check-expect (sum-tree '((3 4) 5)) 12)
;(check-expect (sum-tree 0) 0)
;(check-expect (sum-tree 5) 5)
;(check-expect (sum-tree (list 1 9)) 10)
;(check-expect (sum-tree (list (list 3 4) 2)) 9)
;(check-expect (sum-tree (list 7 (list 12 8))) 27)
;(check-expect (sum-tree (list (list 1 5) (list 6 7))) 19)

;;; Exercise 29.3.2
;(define G
;  (vector (list 1 4)
;          (list 4 5)
;          (list 3)
;          empty
;          (list 2 5)
;          (list 3 6)
;          empty))

;;; find-route : node node graph -> (listof node)
;(define (find-route origination destination G)
;  (find-route-helper origination destination G empty))

;(define (find-neighbors node G)
;  (second (findf (lambda (lst) (symbol=? (first lst) node))
;                 G)))

;(define (find-unvisited-neighbors node G visited)
;  (let [(neighbors (find-neighbors node G))]
;    (filter (lambda (neighbor) (not (member neighbor visited))) neighbors)))

;;; Give a node and a list of lists of nodes, return another list of lists by
;;; prepending node to each sublist.
;(define (augment-list-of-lists node lol)
;  (map (lambda (lst)
;               (cons node lst))
;       lol))

;(define (find-route-helper origination destination G visited)
;  (if (symbol=? origination destination)
;    (list (list destination))
;    (let [(neighbors (find-unvisited-neighbors origination G visited))]
;      (foldr (lambda (neighbor acc)
;                     (let [(nodes (find-route-helper neighbor destination G (cons neighbor visited)))]
;                       (if (list? nodes)
;                         (append (augment-list-of-lists origination nodes) acc)
;                         acc)))
;             empty
;             neighbors))))

;(check-expect '((A B E F G) (A B F G) (A E F G)) (find-route 'A 'G G))
;(check-expect '((A B E C) (A E C)) (find-route 'A 'C G))
;(check-expect empty  (find-route 'I 'G G))

;; sum vector of numbers
(define (vector-sum vec)
  (define (vector-sum-aux i)
    (cond
      [(zero? i) 0]
      [else (+ (vector-ref vec (sub1 i))
               (vector-sum-aux (sub1 i)))]))
  (vector-sum-aux (vector-length vec)))

(check-expect (vector-sum (vector 1 2 3)) 6)
(check-expect (vector-sum (vector)) 0)

(define (vector-sum2 vec)
  (define length (vector-length vec))
  (define (vector-sum-aux i)
    (cond
      [(= i length) 0]
      [else (+ (vector-ref vec i)
               (vector-sum-aux (add1 i)))]))
  (vector-sum-aux 0))

(check-expect (vector-sum2 (vector 1 2 3)) 6)
(check-expect (vector-sum2 (vector)) 0)

;; Exercise 29.3.7
(define (norm vec)
  (sqrt (vector-sum (vector-map (lambda (n) (* n n)) vec))))

(check-within (norm (vector 1 2 3)) (sqrt 14) 0.001)

;; Exercise 29.3.8
(define (vector-contains-doll? vec)
  (define (vector-aux i)
    (cond
      [(= i (vector-length vec)) false]
      [(symbol=? (vector-ref vec i) 'doll) i]
      [else (vector-aux (add1 i))]))
  (vector-aux 0))

(check-expect (vector-contains-doll? (vector 'hello 'world)) false)
(check-expect (vector-contains-doll? (vector 'doll 'world)) 0)
(check-expect (vector-contains-doll? (vector 'hello 'doll)) 1)

;; Exercise 29.3.9
(define (binary-contains sorted-vec key)
  (define (binary-search low high)
    (cond
      [(> low high) false]
      [else (let* [(mid (quotient (+ low high) 2))
                   (value (vector-ref sorted-vec mid))]
              (cond
                [(= key value) mid]
                [(< key value) (binary-search low (sub1 mid))]
                [else (binary-search (add1 mid) high)]))]))
  (binary-search 0 (sub1 (vector-length sorted-vec))))

(check-expect (binary-contains (vector 1 2 3 4 5) 1) 0)
(check-expect (binary-contains (vector 1 2 3 4 5) 3) 2)
(check-expect (binary-contains (vector 1 2 3 4 5) 5) 4)
(check-expect (binary-contains (vector 1 2 3 4 5) 0) false)
(check-expect (binary-contains (vector 1 2 3 4 5) 6) false)

;; Exercise 29.3.10
(define (vector-count vec sym)
  (vector-length (vector-filter (lambda (s) (symbol=? s sym)) vec)))

(check-expect (vector-count (vector 'hello 'world 'yes 'no) 'hello) 1)
(check-expect (vector-count (vector 'hello 'world 'yes 'no) 'hi) 0)
(check-expect (vector-count (vector 'hello 'world 'yes 'no) 'yes) 1)

;; Exercise 29.3.11
(define (id-vector n)
  (build-vector n (lambda (i) 1)))

(check-expect (id-vector 5) #(1 1 1 1 1))
(check-expect (id-vector 0) #())

;; Exercise 29.3.12
(define (my-vector-map v1 v2 f)
  (build-vector (vector-length v1)
                (lambda (i)
                        (f (vector-ref v1 i)
                           (vector-ref v2 i)))))
(define (vector+ v1 v2)
  (my-vector-map v1 v2 +))

(define (vector- v1 v2)
  (my-vector-map v1 v2 -))

(check-expect (vector- #(1 2 3 4 5) #(6 7 8 9 10)) #(-5 -5 -5 -5 -5))
(check-expect (vector- #() #()) #())

;; Exercise 29.3.13
(define (distance v1 v2)
  (sqrt (vector-sum (vector-map (lambda (x) (sqr x)) (vector- v1 v2)))))

(check-within (distance #(1 2 3 4 5) #(6 7 8 9 10)) (sqrt 125) 0.001)

;; Exercise 29.3.14
(define (build-board n f)
  (define (build-row row)
    (build-vector n (lambda (col)
                            (f row col))))
  (build-vector n build-row))

(check-expect (build-board 3 (lambda (i j) 1)) #(#(1 1 1) #(1 1 1) #(1 1 1)))
(check-expect (build-board 3 (lambda (i j) (+ i j))) #(#(0 1 2) #(1 2 3) #(2 3 4)))
(check-expect (build-board 3 (lambda (i j) (* i j))) #(#(0 0 0) #(0 1 2) #(0 2 4)))

(define (board-ref board i j)
  (vector-ref (vector-ref board i) j))

(define Board (build-board 3 (lambda (i j) (+ i j))))
(check-expect (board-ref Board 0 0) 0)
(check-expect (board-ref Board 0 2) 2)
(check-expect (board-ref Board 2 2) 4)
(test)
