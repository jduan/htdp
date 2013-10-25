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
(define Cyclic-Graph
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))
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

;; Exercise 28.1.2
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

(define (test-on-all-nodes graph)
  (let [(nodes (map first graph))]
    (map (lambda (a-node)
                 (map (lambda (b-node)
                              (find-route a-node b-node graph))
                      nodes))
         nodes)))

;; (pretty-print (test-on-all-nodes G))

(check-expect '((A B E F G) (A B F G) (A E C B F G) (A E F G)) (find-route 'A 'G Cyclic-Graph))
(check-expect '((C B E F G) (C B F G)) (find-route 'C 'G Cyclic-Graph))


;; Exercise 28.2.2
(define (build-board n f)
  (build-list n
              (lambda (i)
                      (build-list n (lambda (j)
                                            (f i j))))))

(define (board-ref board i j)
  (list-ref (list-ref board i) j))

(check-expect (build-board 2 (lambda (x y) (odd? (+ x y))))
              (list (list false true)
                    (list true false)))

(check-expect (board-ref (build-board 2 (lambda (x y) (odd? x))) 0 1) false)

(check-expect (board-ref (build-board 2 (lambda (x y) (odd? x))) 1 0) true)
(test)
