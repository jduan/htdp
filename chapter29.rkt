#lang racket
(require test-engine/racket-tests)

;; Exercise 29.1.1
(define (sum-tree tree)
  (cond
    [(number? tree) tree]
    [else (+ (sum-tree (first tree)) (sum-tree (second tree)))]))

(check-expect (sum-tree 3) 3)
(check-expect (sum-tree '((3 4) 5)) 12)
(check-expect (sum-tree 0) 0)
(check-expect (sum-tree 5) 5)
(check-expect (sum-tree (list 1 9)) 10)
(check-expect (sum-tree (list (list 3 4) 2)) 9)
(check-expect (sum-tree (list 7 (list 12 8))) 27)
(check-expect (sum-tree (list (list 1 5) (list 6 7))) 19)

;; Exercise 29.3.2
(define G
  (vector (list 1 4)
          (list 4 5)
          (list 3)
          empty
          (list 2 5)
          (list 3 6)
          empty))

;; find-route : node node graph -> (listof node)
(define (find-route origination destination G)
  (find-route-helper origination destination G empty))

(define (find-neighbors node G)
  (second (findf (lambda (lst) (symbol=? (first lst) node))
                 G)))

(define (find-unvisited-neighbors node G visited)
  (let [(neighbors (find-neighbors node G))]
    (filter (lambda (neighbor) (not (member neighbor visited))) neighbors)))

;; Give a node and a list of lists of nodes, return another list of lists by
;; prepending node to each sublist.
(define (augment-list-of-lists node lol)
  (map (lambda (lst)
               (cons node lst))
       lol))

(define (find-route-helper origination destination G visited)
  (if (symbol=? origination destination)
    (list (list destination))
    (let [(neighbors (find-unvisited-neighbors origination G visited))]
      (foldr (lambda (neighbor acc)
                     (let [(nodes (find-route-helper neighbor destination G (cons neighbor visited)))]
                       (if (list? nodes)
                         (append (augment-list-of-lists origination nodes) acc)
                         acc)))
             empty
             neighbors))))

(check-expect '((A B E F G) (A B F G) (A E F G)) (find-route 'A 'G G))
(check-expect '((A B E C) (A E C)) (find-route 'A 'C G))
(check-expect empty  (find-route 'I 'G G))
(test)