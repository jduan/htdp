;; construct a new list by replacing empty in alon1 by alon2
(define (replace-eol-with alon1 alon2)
  (cond
    [(empty? alon1) alon2]
    [else (cons (first alon1)
                (replace-eol-with (rest alon1) alon2))]))

(equal? (replace-eol-with (list 1 2 3) (list 4 5 6))
        (list 1 2 3 4 5 6))
(equal? (replace-eol-with empty (list 4 5 6))
        (list 4 5 6))

;; Exercise 17.1.1
;; Implement our-append using replace-eol-with
(define (our-append lst1 lst2 lst3)
  (replace-eol-with lst1 (replace-eol-with lst2 lst3)))

(equal? (our-append (list 1 2 3) (list 4 5 6) (list 7 8 9))
        (list 1 2 3 4 5 6 7 8 9))

;; Exercise 17.1.2
;; The function consumes a list of symbols and a list of numbers and produces
;; all the possible pairs of symbols and numbers.
(define (cross los lon)
  (cond
    [(empty? los) empty]
    [else (append (cross-helper (first los) lon)
                  (cross (rest los) lon))]))

(define (cross-helper sym lon)
  (cond
    [(empty? lon) empty]
    [else (cons (list sym (first lon))
                  (cross-helper sym (rest lon)))]))

(equal? (cross '(a b c) '(1 2))
        '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

;; hours->wages : list-of-numbers list-of-numbers  ->  list-of-numbers
;; to construct a new list by multiplying the corresponding items on
;; alon1 and alon2
;; ASSUMPTION: the two lists are of equal length
(define (hours->wages alon1 alon2)
  (cond
    [(empty? alon1) empty]
    [else (cons (* (first alon1) (first alon2))
                (hours->wages (rest alon1) (rest alon2)))]))

(equal? (hours->wages '(40 30 50) '(20 30 40))
        '(800 900 2000))

;; Exercise 17.2.1
;; In the real world, hours->wages consumes lists of employee structures and
;; lists of work structures. An employee structure contains an employee's name,
;; social security number, and pay rate. A work structure contains an
;; employee's name and the number of hours worked in a week. The result is a
;; list of structures that contain the name of the employee and the weekly
;; wage.
(define-struct employee (name ssn pay-rate))
(define-struct work (name hours-worked))

(define (hours->wages2 list-of-employees list-of-work)
  (cond
    [(empty? list-of-employees) empty]
    [else (cons (* (employee-pay-rate (first list-of-employees))
                   (work-hours-worked (first list-of-work)))
                (hours->wages2 (rest list-of-employees) (rest list-of-work)))]))

(define list-of-employees
  (list (make-employee 'jingjing' 123 100)
        (make-employee 'qingqing' 456 120)))
(define list-of-work
  (list (make-work 'jingjing' 40)
        (make-work 'qingqing' 30)))
(equal? (hours->wages2 list-of-employees list-of-work)
        (list 4000 3600))

;; Exercise 17.2.2
(define-struct phone-record (name number))
(define (zip names numbers)
  (cond
    [(empty? names) empty]
    [else
     (cons (make-phone-record (first names) (first numbers))
           (zip (rest names) (rest numbers)))]))

;; list-pick : list-of-symbols N[>= 1]  ->  symbol
;; to determine the nth symbol from alos, counting from 1;
;; signals an error if there is no nth item
(define (list-pick los nth)
  (cond
    [(empty? los) (error 'list-pick "no such element")]
    [(= nth 1) (first los)]
    [else (list-pick (rest los) (sub1 nth))]))

(equal? (list-pick '(hello world you are great) 3) 'you)

;; Exercise 17.3.1
;;  Develop list-pick0, which picks items from a list like list-pick but starts
;;  counting at 0.
(define (list-pick0 los nth)
  (cond
    [(empty? los) (error 'list-pick0 "no such element")]
    [(= nth 0) (first los)]
    [else (list-pick0 (rest los) (sub1 nth))]))

(equal? (list-pick0 '(hello world you are great) 3) 'are)
(equal? (list-pick0 '(hello world you are great) 0) 'hello)

;; Exercise 17.6.1
;; Develop the function merge. It consumes two lists of numbers, sorted in
;; ascending order. It produces a single sorted list of numbers that contains
;; all the numbers on both inputs lists (and nothing else).
(define (merge lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (cond
            [(<= (first lst1) (first lst2)) (cons (first lst1) (merge (rest lst1) lst2))]
            [else (cons (first lst2) (merge lst1 (rest lst2)))])]))

(equal? (merge (list 1 3 5 7 9) (list 2 4 6 8 10))
        (list 1 2 3 4 5 6 7 8 9 10))
(equal? (merge (list 1 8 8 11 12) (list 2 3 4 8 13 14))
        (list 1 2 3 4 8 8 8 11 12 13 14))

;; Exercise 17.6.3
(define-struct punch-card (employee-id hours))
(define-struct employee (name id pay-rate))

;; Insert a number into a list of sorted numbers
;; sort-f is a sort function
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

;; Sort employees
(define (sort-employees aloe)
  (mysort aloe (lambda (e1 e2) (< (employee-id e1) (employee-id e2)))))

(define employees (list (make-employee 'jingjing 2 100)
                        (make-employee 'jessica 3 120)
                        (make-employee 'jake 4 140)
                        (make-employee 'qingqing 1 90)))

(define employees-sorted (sort-employees employees))
(equal? (employee-name (first employees-sorted)) 'qingqing)
(equal? (employee-name (second employees-sorted)) 'jingjing)

;; Sort punch cards
(define (sort-punch-cards cards)
  (mysort cards (lambda (c1 c2) (< (punch-card-employee-id c1)
                                   (punch-card-employee-id c2)))))

(define cards (list (make-punch-card 1 40)
                    (make-punch-card 3 50)
                    (make-punch-card 2 30)))

(define cards-sorted (sort-punch-cards cards))

(equal? (punch-card-employee-id (first cards-sorted)) 1)
(equal? (punch-card-employee-id (second cards-sorted)) 2)

;; calculate wages for a list of employees and their punch cards
(define (hours->wages3 employees cards)
  (hours->wages4 (sort-employees employees) (sort-punch-cards cards)))

;; calculate wages for a list of employees and their punch cards
;; both lists are sorted based on employee ids
(define (hours->wages4 employees cards)
  (cond
    [(empty? cards) empty]
    [(empty? employees) (error 'hours->wages4 "punch cards don't corresponding employees")]
    [(< (punch-card-employee-id (first cards)) (employee-id (first employees)))
     (error 'hours->wages4 "punch card is for an non-existing employee")]
    [(= (punch-card-employee-id (first cards)) (employee-id (first employees)))
     (cons (list (employee-id (first employees)) (* (employee-pay-rate (first employees))
                                                    (punch-card-hours (first cards))))
           (hours->wages4 (rest employees) (rest cards)))]
    [else (hours->wages4 (rest employees) cards)]))

(define wages (hours->wages3 employees cards))
(equal? wages '((1 3600) (2 3000) (3 6000)))

;; Exercise 17.6.4
(define (value coefficients numbers)
  (cond
    [(empty? coefficients) 0]
    [(+ (* (first coefficients) (first numbers))
        (value (rest coefficients) (rest numbers)))]))

(equal? (value '(5 17 3) '(10 1 2)) 73)
(equal? (value '(5 17) '(10 1)) 67)


;; Exercise 17.6.5

;; Given a list of names, return all permutations of the names
(define (arrangements a-word)
  (cond
    [(empty? a-word) (list empty)]
    [else (add-letter-to-list-of-words (first a-word)
                                       (arrangements (rest a-word)))]))
;; add a letter to a list of words
(define (add-letter-to-list-of-words letter low)
  (cond
    [(empty? low) empty]
    [else (append (add-letter-to-word letter (first low))
                  (add-letter-to-list-of-words letter (rest low)))]))
;; add a letter to a word at different locations
(define (add-letter-to-word letter word)
  (cond
    [(empty? word) (list (list letter))]
    [else (cons (cons letter word)
                (add-at-beginning (first word)
                                  (add-letter-to-word letter (rest word))))]))

;; add a letter to the beginning of every word in a list of words
(define (add-at-beginning letter words)
  (cond
    [(empty? words) empty]
    [else (cons (cons letter (first words))
                (add-at-beginning letter (rest words)))]))

(equal? (arrangements '(Louise Jane Laura))
        '((Louise Jane Laura)
            (Jane Louise Laura)
            (Jane Laura Louise)
            (Louise Laura Jane)
            (Laura Louise Jane)
            (Laura Jane Louise)))

;; non-same : list-of-names list-of-list-of-names  ->  list-of-list-of-names,
;; which consumes a list of names L and a list of arrangements and produces the
;; list of those that do not agree with L at any position.
(define (non-same list-of-names list-of-list-of-names)
  (cond
    [(empty? list-of-list-of-names) empty]
    [(dont-agree list-of-names (first list-of-list-of-names))
     (cons (first list-of-list-of-names) (non-same list-of-names (rest list-of-list-of-names)))]
    [else (non-same list-of-names (rest list-of-list-of-names))]))

;; dont-agree: list-of-names1 list-of-names2 -> boolean
;; check if two lists of names don't agree with each other at any position
(define (dont-agree list-of-names1 list-of-names2)
  (cond
    [(empty? list-of-names1) true]
    [(symbol=? (first list-of-names1) (first list-of-names2)) false]
    [else (dont-agree (rest list-of-names1) (rest list-of-names2))]))

(equal? (non-same
          (list 'Carol 'Mary 'John)
          (list (list 'Mary 'John 'Carol)
                (list 'Mary 'Carol 'John)
                (list 'John 'Carol 'Mary)
                (list 'John 'Mary 'Carol)
                (list 'Carol 'John 'Mary)
                (list 'Carol 'Mary 'John)))
        '((Mary John Carol) (John Carol Mary)))

;; random-pick : list-of-list-of-names  ->  list-of-names, which consumes a list
;; of items and randomly picks one of them as the result;
(define (random-pick list-of-list-of-names)
  (pick-nth list-of-list-of-names (random (length list-of-list-of-names))))

(define (pick-nth lst n)
  (if (zero? n)
    (first lst)
    (pick-nth (rest lst) (sub1 n))))

(random-pick (list (list 'John 'Carol 'Ron 'Mary)
                   (list 'Mary 'Carol 'Ron 'John)
                   (list 'Mary 'Ron 'Carol 'John)
                   (list 'Mary 'John 'Ron 'Carol)
                   (list 'John 'Ron 'Carol 'Mary)
                   (list 'Ron 'John 'Carol 'Mary)
                   (list 'Ron 'Carol 'Mary 'John)
                   (list 'John 'Ron 'Mary 'Carol)
                   (list 'John 'Ron 'Carol 'Mary)
                   (list 'Ron 'John 'Mary 'Carol)))

(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))

(gift-pick '(John Carol Ron Mary))

;; Exercise 17.6.6
;; The function takes two arguments, both lists of symbols (only 'a,
;; 'c, 'g, and 't occur in DNA, but we can safely ignore this issue here).
;; The first list is called a pattern, the second one a search-string. The
;; function returns true if the pattern is a prefix of the search-string.
;; In all other cases, the function returns false.
(define (DNAprefix pattern search-string)
  (cond
    [(empty? pattern) true]
    [(empty? search-string) false]
    [(symbol=? (first pattern) (first search-string))
     (DNAprefix (rest pattern) (rest search-string))]
    [else false]))

(DNAprefix (list 'a 't) (list 'a 't 'c))
(not (DNAprefix (list 'a 't) (list 'a)))
(DNAprefix (list 'a 't) (list 'a 't))
(not (DNAprefix (list 'a 'c 'g 't) (list 'a 'g)))
(not (DNAprefix (list 'a 'a 'c 'c) (list 'a 'c)))

;; Modify DNAprefix so that it returns the first item beyond the pattern in the
;; search-string if the pattern is a proper prefix of the search-string. If the
;; lists do not match or if the pattern is no shorter than the search-string,
;; the modified function should still return false. Similarly, if the lists are
;; equally long and match, the result is still true.
(define (DNAprefix2 pattern search-string)
  (cond
    [(empty? pattern)
     (cond
       [(empty? search-string) true]
       [else (first search-string)])]
    [(empty? search-string) false]
    [(symbol=? (first pattern) (first search-string))
     (DNAprefix2 (rest pattern) (rest search-string))]
    [else false]))

(DNAprefix2 empty empty)
(equal? (DNAprefix2 empty '(a t)) 'a)
(equal? (DNAprefix2 (list 'a 't 'c) empty) false)
(equal? (DNAprefix2 (list 'a 't) (list 'a 't)) true)
(equal? (DNAprefix2 (list 'a 't) (list 'a 't 'c 'g)) 'c)
(equal? (DNAprefix2 (list 'a 't 'c) (list 'a 't)) false)
(equal? (DNAprefix2 (list 'a 'c) (list 'a 't)) false)
(equal? (DNAprefix2 (list 'a) (list 'a 't 'c 'g)) 't)

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

;; Determine if two web pages are the same
(define (web=? web1 web2)
  (cond
    [(empty? web1) (empty? web2)]
    [(symbol? (first web1))
     (and (cons? web2)
          (symbol? (first web2))
          (symbol=? (first web1) (first web2))
          (web=? (rest web1) (rest web2)))]
    [else (and (cons? web2)
               (list? (first web2))
               (web=? (first web1) (first web2))
               (web=? (rest web1) (rest web2)))]))

;combination 1
(equal? (web=? empty empty) true)

;combination 2
(equal? (web=? (list 'Hello 'World) empty) false)

;combination 3
(equal? (web=? (list (list 'Goodbye)) empty) false)

;combination 4
(equal? (web=? empty (list 'Homework)) false)

;combination 5
(equal? (web=? (list 'Hello 'World) (list 'Homework)) false)

;combination 5
(equal? (web=? (list 'Homework) (list 'Homework)) true)

;combination 6
(equal? (web=? (list (list 'Goodbye)) (list 'Homework)) false)

;combination 7
(equal? (web=? empty (list (list 'Solutions))) false)

;combination 8
(equal? (web=? (list 'Hello 'World) (list (list 'Solutions))) false)

;combination 9
(equal? (web=? (list (list 'Solutions)) (list (list 'Solutions))) true)

;combination 9
(equal? (web=? (list (list 'Goodbye)) (list (list 'Solutions))) false)


;; Exercise 17.8.7
(define-struct posn (x y))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;Examples as Tests:
(equal?  (posn=? (make-posn 8 12) (make-posn 8 12)) true)

(equal?  (posn=? (make-posn 7 12) (make-posn 8 12)) false)

(equal?  (posn=? (make-posn 8 3) (make-posn 8 12)) false)

(equal?  (posn=? (make-posn 8 3) (make-posn 3 8)) false)

(equal?  (posn=? (make-posn 3 5) (make-posn 3 5)) true)

;; Exercise 17.8.8
;; Determine if two binary trees are the same.
;; A binary tree is either false or a structure:
(define-struct node (value left right))
(define (tree=? t1 t2)
  (cond
    [(false? t1) (false? t2)]
    [else (and (node? t2)
          (= (node-value t1) (node-value t2))
          (tree=? (node-left t1) (node-left t2))
          (tree=? (node-right t1) (node-right t2)))]))

;Examples as Tests:
(equal?
 (tree=? false false)
 true)

(equal?
 (tree=? (make-node 12
                    false
                    false)
         (make-node 12
                    false
                    false))
 true)

(equal?
 (tree=? false false)
 true)

(equal?
 (tree=? (make-node 12
                    false
                    false)
         (make-node 18
                    false
                    false))
 false)

(equal?
 (tree=?
  (make-node 12
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   false
                                   false)
                        false))
  (make-node 12
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   false
                                   false)
                        false)))
 true)

(equal?
 (tree=?
  (make-node 12
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   false
                                   false)
                        false))
  (make-node 12
             (make-node 35
                        false
                        false)
             (make-node 65
                        (make-node 89
                                   (make-node 7
                                              false
                                              false)
                                   false)
                        false)))
 false)

;; Exercise 17.8.9
;; Determine if two s-lists are the same.
(define (slist=? slist1 slist2)
  (cond
    [(empty? slist1) (empty? slist2)]
    [else (and (cons? slist2)
               (s-exp=? (first slist1) (first slist2))
               (slist=? (rest slist1) (rest slist2)))]))

;; Determine if two s-exps are the same.
(define (s-exp=? exp1 exp2)
  (cond
    [(number? exp1) (and (number? exp2) (= exp1 exp2))]
    [(boolean? exp1) (and (boolean? exp2) (boolean=? exp1 exp2))]
    [(symbol? exp1) (and (symbol? exp2) (symbol=? exp1 exp2))]
    [else (slist=? exp1 exp2)]))

;Examples as Tests:
(equal?
 (slist=?
  empty
  empty)
 true)

(equal?
 (slist=?
  empty
  (list 'w 6 6 false))
 false)

(equal?
 (slist=?
  (list 'w 6 6 false)
  empty)
 false)

(equal?
 (slist=?
  (list 'w 6 6 false)
  (list 'w 6 6 false))
 true)

(equal?
 (slist=?
  (list 'w 6 6 false)
  (list 'w 6 false))
 false)

(equal?
 (slist=?
  (list 'w 6 6 false (list true 'a 7 'b))
  (list 'w 6 6 false (list true 'a 7 'b)))
 true)

(equal?
 (slist=?
  (list 'w 6 6 false (list true 'a 7 'b))
  (list 'w 6 (list true 'a 7 'b) 6 false ))
 false)

(equal?
 (slist=?
  (list 'w (list 5) 6 false)
  (list 'w (list 5) 6 false))
 true)
