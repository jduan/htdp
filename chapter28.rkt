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

;; Exercise 28.2.3
(define-struct posn (x y))
(define (threatened? queen-pos another-pos)
  (or (= (posn-x queen-pos) (posn-x another-pos))
      (= (posn-y queen-pos) (posn-y another-pos))
      (let [(slope (/ (- (posn-x queen-pos) (posn-x another-pos))
                      (- (posn-y queen-pos) (posn-y another-pos))))]
        (or (= 1 slope)
            (= -1 slope)))))

;; same position
(check-expect (threatened? (make-posn 0 0) (make-posn 0 0)) true)

;; same vertical
(check-expect (threatened? (make-posn 0 0) (make-posn 0 1)) true)

;; same horizontal
(check-expect (threatened? (make-posn 0 0) (make-posn 1 0)) true)

;; same diagonal (up and left)
(check-expect (threatened? (make-posn 1 1) (make-posn 2 2)) true)
(check-expect (threatened? (make-posn 2 1) (make-posn 3 2)) true)
(check-expect (threatened? (make-posn 5 8) (make-posn 9 12)) true)

;; same diagonal (down and right)
(check-expect (threatened? (make-posn 3 3) (make-posn 4 2)) true)
(check-expect (threatened? (make-posn 5 8) (make-posn 1 12)) true)

;; failure
(check-expect (threatened? (make-posn 0 0) (make-posn 2 3)) false)

;; Exercise 28.2.4
(define (placement n board)
  (placement-helper n board empty))

(define (update-board board pos value)
  (for/list ([i (board-dim board)]
             [lst board])
            (if (= i (posn-x pos))
              (list-update lst (posn-y pos) value)
              lst)))

(define (list-update lst idx value)
  (for/list ([i (length lst)]
             [element lst])
            (if (= i idx)
              value
              element)))

;; The cache is used to avoid repeated computation because the problem space can
;; be quite large even for 8 queens!
(define Cache (make-hash))

(define (placement-helper n board list-of-queens)
  (if (hash-has-key? Cache board)
    (hash-ref Cache board)
    (if (= n (length list-of-queens))
      (list board)
      (let* [(positions (all-positions board))
             (candidates (filter (lambda (p) (not  (threatened2? p list-of-queens)))
                                 positions))
             (boards (remove-duplicates
                       (foldl
                         (lambda (candidate acc)
                                 (let [(results (placement-helper n
                                                                  (update-board board candidate false)
                                                                  (cons candidate list-of-queens)))]
                                   (append results acc))
                                 )
                         empty
                         candidates)))]
        (hash-set! Cache board boards)
        boards))))

(define (all-positions board)
  (for*/list ([i (board-dim board)]
              [j (board-dim board)])
             (make-posn i j)
             ))

;; Given a list of queens, figure out if you can place another queen at
;; position p.
(define (threatened2? p queens)
  (>
    (length
      (filter (lambda (b) b)
              (map (lambda (q) (threatened? q p)) queens)))
    0))

;; return the dimention of a board
(define (board-dim board)
  (length board))

(define Board-Size 8)
(define Board (build-board Board-Size (lambda (i j) true)))
(define solutions (placement Board-Size Board))
;; There are 92 solutions for the 8-queen problem
;; http://en.wikipedia.org/wiki/Eight_queens_puzzle
(check-expect (length solutions) 92)
(pretty-print solutions)
(test)
