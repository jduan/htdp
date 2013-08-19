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

(define (sort-n alon)
  (mysort alon <))

(equal? '(1 2 3 4 5 6) (sort-n '(3 4 2 1 6 5)))

;; Exercise 12.2.1
(define-struct mail (name date message))

;; a function that compares 2 mail messages by date
(define (compare-mail-by-date mail1 mail2)
  (< (mail-date mail1) (mail-date mail2)))

;; sort a list of mail messages by date
(define (sort-mail-by-date alom)
  (mysort alom compare-mail-by-date))

;; a function that compares 2 mail messages by name
(define (compare-mail-by-name mail1 mail2)
  (string<? (mail-name mail1) (mail-name mail2)))

;; sort a list of mail messages by name
(define (sort-mail-by-name alom)
  (mysort alom compare-mail-by-name))

(define example-list-of-mail-messages
  (cons
    (make-mail "Ronnie" 1980 "Don't forget my number!")
    (cons
      (make-mail "Richard" 1960 "I am not a crook")
      (cons
        (make-mail "George" 1990 "No new taxes")
        empty))))
(define example-list-of-mail-messages-sorted-by-date
  (sort-mail-by-date example-list-of-mail-messages))

(define example-list-of-mail-messages-sorted-by-name
  (sort-mail-by-name example-list-of-mail-messages))

(mail-name (first example-list-of-mail-messages-sorted-by-name))

;; check if a number occurs in a sorted list of numbers
(define (search-sorted n alon)
  (if (empty? alon)
    false
    (cond
      [(< n (first alon)) false]
      [else (or
              (= n (first alon))
              (search-sorted n (rest alon)))])))

(search-sorted 2 '(1 2 3 4))
(search-sorted 2 '(1 3 4))

;; Add an object to the end of a list
(define (add-at-end obj lst)
  (if (empty? lst)
    (cons obj empty)
    (cons (first lst)
          (add-at-end obj (rest lst)))))

(equal? (add-at-end 6 '(1 2 3 4 5)) '(1 2 3 4 5 6))
