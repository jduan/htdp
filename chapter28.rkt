#lang racket
(require test-engine/racket-tests)

(define G
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())
    (H (C))
    (I (H))))
;; Exercise 28.1.1
(check-expect
  G
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D empty)
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G empty)
        (list 'H (list 'C))
        (list 'I (list 'H))))

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
