(define-struct child (father mother name date eyes))

;; oldest generation
(define Carl (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

;; middle generation
(define Adam (make-child Carl Bettina 'Adam 1950 'yellow))
(define Dave (make-child Carl Bettina 'Dave 1955 'black))
(define Eva (make-child Carl Bettina 'Eva 1965 'blue))
(define Fred (make-child empty empty 'Fred 1966 'pink))

;; youngest generation
(define Gustav (make-child Fred Eva 'Gustav 1988 'brown))

(define (blue-eyed-ancestor? child)
  (cond
    [(empty? child) false]
    [else (or (symbol=? 'blue (child-eyes child))
              (blue-eyed-ancestor? (child-father child))
              (blue-eyed-ancestor? (child-mother child)))]))

(equal? false (blue-eyed-ancestor? Carl))
(equal? true (blue-eyed-ancestor? Gustav))

;; Exercise 14.1.3
(define (count-persons node)
  (cond
    [(empty? node) 0]
    [else (+ 1
             (count-persons (child-father node))
             (count-persons (child-mother node)))]))

(equal? 0 (count-persons empty))
(equal? 1 (count-persons Carl))
(equal? 1 (count-persons Fred))
(equal? 3 (count-persons Adam))
(equal? 5 (count-persons Gustav))

;; Exercise 14.1.4
(define (add-all-ages node current-year)
  (cond
    [(empty? node) 0]
    [else (+ (add-all-ages (child-father node) current-year)
             (add-all-ages (child-mother node) current-year)
             (- current-year (child-date node)))]))

;; Doesn't handle empty node!
(define (average-age node current-year)
  (/ (add-all-ages node current-year)
     (count-persons node)))

(equal? 75 (average-age Carl 2001))
(equal? 75 (average-age Bettina 2001))
(equal? 67 (average-age Adam 2001))
(equal? (+ 65 1/3) (average-age Dave 2001))
(equal? 62 (average-age Eva 2001))
(equal? 35 (average-age Fred 2001))
(equal? (+ 46 4/5) (average-age Gustav 2001))

;; Exercise 14.1.5
(define (eye-colors node)
  (cond
    [(empty? node) empty]
    [else (cons (child-eyes node)
                (append (eye-colors (child-father node))
                        (eye-colors (child-mother node))))]))

(equal? empty (eye-colors empty))
(equal? (list 'green) (eye-colors Carl ))
(equal? (list 'green) (eye-colors Bettina ))
(equal? (list 'yellow 'green 'green) (eye-colors Adam ))
(equal? (list 'black 'green 'green) (eye-colors Dave ))
(equal? (list 'blue 'green 'green) (eye-colors Eva ))
(equal? (list 'pink) (eye-colors Fred ))
(equal? (list 'brown 'pink 'blue 'green 'green) (eye-colors Gustav ))

;; Exercise 14.1.6
(define (parent-blue-eyed? child)
  (or
    (and (not (empty? (child-father child))) (symbol=? 'blue (child-eyes (child-father child))))
    (and (not (empty? (child-mother child))) (symbol=? 'blue (child-eyes (child-mother child))))))

(equal? true (parent-blue-eyed? Gustav))

(define (proper-blue-eyed-ancestor? child)
  (cond
    [(empty? child) false]
    [else (or (parent-blue-eyed? child)
              (proper-blue-eyed-ancestor? (child-father child))
              (proper-blue-eyed-ancestor? (child-mother child)))]))

;; solution 2
(define (proper-blue-eyed-ancestor? child)
  (and
    (blue-eyed-ancestor? child)
    (not (symbol=? 'blue (child-eyes child)))))

(equal? true (proper-blue-eyed-ancestor? Gustav))
(equal? false (proper-blue-eyed-ancestor? Eva))
(equal? false (proper-blue-eyed-ancestor? Carl))


;; section 14.2

(define-struct node (ssn name left right))

;; Exercise 14.2.1
(define (contains-bt ssn tree)
  (cond
    [(false? tree) false]
    [else (or (= ssn (node-ssn tree))
              (contains-bt ssn (node-left tree))
              (contains-bt ssn (node-right tree)))]))

;; Exercise 14.2.2
(define (search-bt ssn tree)
  (cond
    [(false? tree) false]
    [else (cond
            [(= ssn (node-ssn tree)) (node-name tree)]
            [else (or
                    (search-bt ssn (node-left tree))
                    (search-bt ssn (node-right tree)))])]))

;; Exercise 14.2.3
(define (inorder tree)
  (cond
    [(boolean? tree) empty]
    [else (append
            (inorder (node-left tree))
            (cons (node-ssn tree) (inorder (node-right tree))))]))

(inorder (make-node 66 'a
                    (make-node 53 'b false false)
                    (make-node 78 'c false false)))

;; Exercise 14.2.4
(define (search-bst ssn tree)
  (cond
    [(boolean? tree) false]
    [else (cond
            [(= ssn (node-ssn tree)) (node-name tree)]
            [(< ssn (node-ssn tree))
             (search-bst ssn (node-left tree))]
            [else
             (search-bst ssn (node-right tree))])]))

;; Exercise 14.2.5
(define (create-bst tree ssn name)
  (cond
    [(boolean? tree) (make-node ssn name false false)]
    [else (cond
            [(< ssn (node-ssn tree))
             (make-node (node-ssn tree) (node-name tree)
                        (create-bst (node-left tree) ssn name)
                        (node-right tree))]
            [(> ssn (node-ssn tree))
             (make-node (node-ssn tree) (node-name tree)
                        (node-left tree)
                        (create-bst (node-right tree) ssn name))]
            [else (error 'create-bst "Number already in BST")])]))

;; Exercise 14.2.6
(define (create-bst-from-list lonn)
  (cond
    [(empty? lonn) (create-bst false)]
    [else (create-bst (create-bst-from-list (rest lonn))
                      (first (first lonn))
                      (second (first lonn)))]))

(define (size a-wp)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (+ 1 (size (rest a-wp)))]
    [(list? (first a-wp)) (+ (size (first a-wp))
                             (size (rest a-wp)))]))

(equal? 29
        (size '(The TeachScheme Web Page
                    Here you can find:
                    (LectureNotes for Teachers)
                    (Guidance for (DrScheme: a Scheme programming environment))
                    (Exercise Sets)
                    (Solutions for Exercises)
                    For further information: write to scheme@cs)))

;; Exercise 14.3.2
(define (occurs1 a-wp word)
  (cond
    [(empty? a-wp) 0]
    [(and (symbol? (first a-wp))
          (symbol=? word (first a-wp)))
     (+ 1 (occurs1 (rest a-wp) word))]
    [else (occurs1 (rest a-wp) word)]))

(= (occurs1 empty 'a) 0)
(= (occurs1 '(a b a) 'a) 2)
(= (occurs1 '((a b c) b (a a a) a) 'a) 1)

(define (occurs2 a-wp word)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp))
     (cond
       [(symbol=? word (first a-wp)) (+ 1 (occurs2 (rest a-wp) word))]
       [else (occurs2 (rest a-wp) word)])]
    [else (+ (occurs2 (first a-wp) word)
             (occurs2 (rest a-wp) word))]))

(= (occurs2 empty 'a) 0)
(= (occurs2 '(a b a) 'a) 2)
(= (occurs2 '((a b c) b (a a a) a) 'a) 5)

;; Exercise 14.3.3
(define (replace new old wp)
  (cond
    [(empty? wp) empty]
    [(symbol? (first wp))
     (cond
       [(symbol=? (first wp) old) (cons new (replace new old (rest wp)))]
       [else (cons (first wp) (replace new old (rest wp)))])]
    [else (cons (replace new old (first wp))
                (replace new old (rest wp)))]))

(equal? (replace 'new 'old empty) empty)
(equal? (replace 'new 'old '(other)) (list 'other))
(equal? (replace 'new 'old '(old)) (list 'new))
(equal? (replace 'new 'old '((new old other) other old old))
        (list (list 'new 'new 'other) 'other 'new 'new))

;; Exercise 14.3.4
(define (depth wp)
  (cond
    [(empty? wp) 0]
    [(symbol? (first wp)) (depth (rest wp))]
    [else (max (+ 1 (depth (first wp)))
               (depth (rest wp)))]))

(equal? (depth '()) 0)
(equal? (depth '(a)) 0)
(equal? (depth '(())) 1)
(equal? (depth '(a b c)) 0)
(equal? (depth '(a (b (c (d))) e (f (g)) h)) 3)

;; Exercise 14.4.1
;; (make-add 10 -10)
;; (make-add (make-mul 20 3) 33)
;; (make-mul 3.14 (make-mul 'r 'r))
;; (make-add (make-mul 9/5 'c) 32)
;; (make-add (make-mul 3.14 (make-mul 'o 'o))
;;           (make-mul 3.14 (make-mul 'i 'i)))
;;

;; Exercise 14.4.2
(define-struct add (lhs rhs))
(define-struct mul (lhs rhs))

(define (numeric? exp)
  (cond
    [(number? exp) true]
    [(symbol? exp) false]
    [(add? exp) (and (numeric? (add-lhs exp))
                     (numeric? (add-rhs exp)))]
    [(mul? exp) (and (numeric? (mul-lhs exp))
                     (numeric? (mul-rhs exp)))]))

(equal? true (numeric? (make-add 10 -10)))
(equal? true (numeric? (make-add (make-mul 20 3) 33)))
(equal? false (numeric? (make-add
                          (make-mul 3.14 (make-mul 'o 'o))
                          (make-mul 3.14 (make-mul 'i 'i)))))

;; Exercise 14.4.3
(define (evaluate-expression exp)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (error 'evaluate-expression "got a variable")]
    [(add? exp) (+ (evaluate-expression (add-lhs exp))
                     (evaluate-expression (add-rhs exp)))]
    [(mul? exp) (* (evaluate-expression (mul-lhs exp))
                     (evaluate-expression (mul-rhs exp)))]))

(equal? 24 (evaluate-expression (make-add (make-add 5 7) (make-mul 4 3))))
(equal? 8 (evaluate-expression (make-mul 4 2)))

;; Exercise 14.4.4
(define (subst V N exp)
  (cond
    [(number? exp) exp]
    [(symbol? exp)
     (cond
       [(symbol=? exp V) N]
       [else exp])]
    [(add? exp) (make-add (subst V N (add-lhs exp))
                          (subst V N (add-rhs exp)))]
    [(mul? exp) (make-mul (subst V N (mul-lhs exp))
                          (subst V N (mul-rhs exp)))]))

(equal? (subst 'x 2 1) 1)
(equal? (subst 'x 2 'x) 2)
(equal? (subst 'y 2 'x) 'x)

;; 17.8
;; Compare if two lists of numbers are the same.
(define (list=? lst1 lst2 cmp)
  (cond
    [(and (empty? lst1) (empty? lst2)) true]
    [(and (not (empty? lst1)) (not (empty? lst2)))
     (and (cmp (first lst1) (first lst2))
          (list=? (rest lst1) (rest lst2) cmp))]
    [else false]))

(define (num-list=? lst1 lst2)
  (list=? lst1 lst2 =))

(equal? (num-list=? empty empty) true)
(equal? (num-list=? empty '(1)) false)
(equal? (num-list=? '(1) empty) false)
(equal? (num-list=? '(1 2 3) '(4 5 6)) false)
(equal? (num-list=? '(1 2 3) '(1 2 3)) true)
(equal? (num-list=? '(2 1 3) '(1 2 3)) false)

;; Exercise 17.8.3
;; Determine if two lists of symbols are the same.
(define (sym-list=? lst1 lst2)
  (list=? lst1 lst2 symbol=?))

(equal? (sym-list=? empty empty) true)
(equal? (sym-list=? empty '(a)) false)
(equal? (sym-list=? '(a) empty) false)
(equal? (sym-list=? '(a b c) '(a b c)) true)
(equal? (sym-list=? '(b c) '(a b c)) false)
(equal? (sym-list=? '(a c b) '(a b c)) false)

;; Exercise 17.8.4
;; Develop contains-same-numbers. The function determines whether two lists of
;; numbers contain the same numbers, regardless of the ordering.
(define (contains-same-numbers nums1 nums2)
  (list=? (remove-duplicates (sort nums1 <)) (remove-duplicates (sort nums2 <)) =))

;; Remove duplicate numbers from a list of sorted numbers.
(define (remove-duplicates nums)
  (cond
    [(empty? nums) empty]
    [(> (length nums) 1)
     (cond
       [(= (first nums) (second nums)) (remove-duplicates (rest nums))]
       [else (cons (first nums) (remove-duplicates (rest nums)))])]
    [else nums]))

(equal? (contains-same-numbers '(1 2 3) '(3 2 1)) true)
(equal? (contains-same-numbers '(1 2 3 4) '(3 2 1)) false)
(equal? (contains-same-numbers '(1 2 3 3 2 1) '(3 2 1)) true)
(equal? (contains-same-numbers '(1 1 1 1) '(1)) true)

;; Exercise 17.8.5
;;  The class of numbers, symbols, and booleans are sometimes called atoms
;;  Develop the function list-equal?, which consumes two lists of atoms and
;;  determines whether they are equal.
(define (list-equal? atoms1 atoms2)
  (cond
    [(empty? atoms1) (empty? atoms2)]
    [else
     (and (cons? atoms2)
          (atom-equal? (first atoms1) (first atoms2))
          (list-equal? (rest atoms1) (rest atoms2)))]))

(define (atom-equal? atom1 atom2)
  (cond
    [(and (number? atom1) (number? atom2)) (= atom1 atom2)]
    [(and (boolean? atom1) (boolean? atom2)) (boolean=? atom1 atom2)]
    [(and (symbol? atom1) (symbol? atom2)) (symbol=? atom1 atom2)]
    [else false]))

(equal? (list-equal? empty empty) true)
(equal? (list-equal? empty (list 1 false 'a)) false)
(equal? (list-equal? (list 1 false 'a) empty) false)
(equal? (list-equal? (list 1 false 'a) (list 1 false 'a)) true)
(equal? (list-equal? (list 1 false 'a) (list 'a 1 false)) false)
(equal? (list-equal? (list 1 false 'a) (list 3 false 'a)) false)
