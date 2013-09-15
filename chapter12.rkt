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

;; to create a list of all re-arrangements of the letters in a word
;; a word is a list of symbols
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

(add-at-beginning 'a '((b c) (c b)))
(add-letter-to-word 'a '(b))
(add-letter-to-list-of-words 'a '((b)))
(arrangements empty)
(arrangements '(a))
(arrangements '(a b))
(arrangements '(a b c))

;; Exercise 18.1.10
(define-struct parent (children name date eyes))

(define Gustav (make-parent empty 'Gustav 1988 'brown))
(define Eva (make-parent (list Gustav) 'Eva 1965 'blue))
(define Fred (make-parent (list Gustav) 'Fred 1966 'pink))
(define Adam (make-parent empty 'Adam 1950 'yellow))
(define Dave (make-parent empty 'Dave 1955 'black))
(define Carl (make-parent (list Adam Dave Eva) 'Carl 1926 'green))
(define Bettina (make-parent (list Adam Dave Eva) 'Bettina 1926 'green))

(define (blue-eyed-descendant? parent)
  (local [(define (blue-eyed-descendant? parent)
            (or
              (symbol=? 'blue (parent-eyes parent))
              (blue-eyed-children? (parent-children parent))))
            (define (blue-eyed-children? children)
              (cond
                [(empty? children) false]
                [else (or (blue-eyed-descendant? (first children))
                          (blue-eyed-children? (rest children)))]))]
         (blue-eyed-descendant? parent)))

(equal? false (blue-eyed-descendant? Gustav))
(equal? false (blue-eyed-descendant? Fred))
(equal? true (blue-eyed-descendant? Eva))
(equal? false (blue-eyed-descendant? Dave))
(equal? false (blue-eyed-descendant? Adam))
(equal? true (blue-eyed-descendant? Bettina))
(equal? true (blue-eyed-descendant? Carl))


;; A rock start has a name and use some instrument
(define-struct star (name instrument))
(define alos
  (list (make-star 'Chris 'saxophone)
        (make-star 'Robby 'trumpet)
        (make-star 'Matt 'violin)
        (make-star 'Wen 'guitar)
        (make-star 'Matt 'radio)))
;; last-occurrence : symbol list-of-star  ->  star or false
;; to find the last star record in alostars that contains s in name field
(define (last-occurrence s alostars)
  (local [(define (find-all name stars)
            (cond
              [(empty? stars) empty]
              [(symbol=? name (star-name (first stars)))
               (cons (first stars) (find-all name (rest stars)))]
              [else (find-all name (rest stars))]))
          (define (last-occurrence name stars)
            (local ((define selected (find-all name stars)))
                   (if (empty? selected)
                     false
                     (star-instrument (last (find-all name stars))))))]
         (last-occurrence s alostars)))

(equal? 'radio (last-occurrence 'Matt alos))
(equal? 'guitar (last-occurrence 'Wen alos))
(equal? 'trumpet (last-occurrence 'Robby alos))
(equal? false (last-occurrence 'Jingjing alos))

;; Determine the largest number of a list of numbers
(define (maxi nums)
  (cond
    [(empty? (rest nums)) (first nums)]
    [else (local ((define rest-max (maxi (rest nums))))
                (if (> (first nums) rest-max)
                  (first nums)
                  rest-max))]))

(equal? 20 (maxi (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(equal? 30 (maxi (list 1 2 3 4 5 6 7 8 9 10 11 30 13 14 15 16 17 18 19 20)))

;; Exercise 18.1.13
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
(define Django (make-child empty Eva 'Django 1990 'grey))

(define Hal (make-child Gustav empty 'Gustav 1988 'hazel))

;; to-blue-eyed-ancestor : ftn  ->  path or false
;; to compute the path from a-ftn tree to a blue-eyed ancestor
;; A path is a list of 'father and 'mother. The empty path indicates that a-ftn
;; has 'blue in the eyes field'
(define (to-blue-eyed-ancestor a-ftn)
  (local [(define ancestor-father-side
            (cond
              [(child? (child-father a-ftn)) (to-blue-eyed-ancestor (child-father a-ftn))]
              [else false]))
          (define ancestor-mother-side
            (cond
              [(child? (child-mother a-ftn)) (to-blue-eyed-ancestor (child-mother a-ftn))]
              [else false]))]
         (cond
           [(symbol=? 'blue (child-eyes a-ftn)) empty]
           [(list? ancestor-father-side) (cons 'father ancestor-father-side)]
           [(list? ancestor-mother-side) (cons 'mother ancestor-mother-side)]
           [else false])))

(equal? empty (to-blue-eyed-ancestor Eva))
(equal? false (to-blue-eyed-ancestor Fred))
(equal? '(mother) (to-blue-eyed-ancestor Gustav))
(equal? false (to-blue-eyed-ancestor Dave))
(equal? false (to-blue-eyed-ancestor Adam))
(equal? false (to-blue-eyed-ancestor Bettina))
(equal? false (to-blue-eyed-ancestor Carl))
(equal? '(father mother) (to-blue-eyed-ancestor Hal))
