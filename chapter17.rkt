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
