;; Exercise 22.2.1
;; (X -> y) -> ((listof X) -> (listof Y))
;; Consumes a convert function and returns a function that takes a list of
;; items and applies the convert function.
(define (mymap converter)
  (local ((define (abs-fun lst)
            (map converter lst)))
         abs-fun))

(define (C->F degree) (+ 32 (* degree 9/5)))
((mymap C->F) '(0 32 100))

(define-struct ir (name price))
(define inventory-records
  (list (make-ir 'book 25)
        (make-ir 'video 5)
        (make-ir 'audio 7)
        (make-ir 'ce 100)))
(equal? '(book video audio ce)((mymap ir-name) inventory-records))

;; Exercise 22.2.2
;; (X Y -> boolean) -> ((listof X) -> (listof X))
;; Consumes a comparison function and returns a function that takes a list of
;; items and sort them using the comparison function.
(define (mysort cmp)
  (local ([define (mysort alon)
           (cond
             [(empty? alon) alon]
             [else (insert (first alon) (mysort (rest alon)))])]
          [define (insert an alon)
           (cond
             [(empty? alon) (list an)]
             [else (cond
                     [(cmp an (first alon)) (cons an alon)]
                     [else (cons (first alon) (insert an (rest alon)))])])])
         mysort))

(equal? ((mysort <) '(2 3 1 5 4)) '(1 2 3 4 5))
(equal? ((mysort >) '(2 3 1 5 4)) '(5 4 3 2 1))

;; Exercise 22.2.3
;; (X Y ... Z -> Z') init -> ((listof X) -> value)
;; Consumes a combine function and an initial value, and returns a function that
;; takes a list of items and "fold" them into a final value.
;; my own implementation of fold right
(define (myfoldr combine init)
  (local ([define (myfoldr lst)
           (cond
             [(empty? lst) init]
             [else (combine (first lst) (myfoldr (rest lst)))])])
         myfoldr))

(equal? ((myfoldr + 0) '(1 2 3 4)) 10)
(equal? ((myfoldr - 0) '(1 2 3 4)) -2)
(equal? ((myfoldr * 1) '(1 2 3 4)) 24)
(equal? ((myfoldr / 1) '(1 2 3 4)) 3/8)
(equal? ((myfoldr cons '(1 2 3)) '(4 5 6)) '(4 5 6 1 2 3))

;; my own implementation of fold left
(define (myfoldl combine init)
  (local ([define (myfoldl lst acc)
           (cond
             [(empty? lst) acc]
             [else (myfoldl (rest lst) (combine (first lst) acc))])]
          [define (myfoldl2 lst)
           (myfoldl lst init)])
          myfoldl2))

(equal? ((myfoldl - 0) '(1 2 3 4)) 2)
(equal? ((myfoldl / 1) '(1 2 3 4)) 8/3)
(equal? ((myfoldl cons '(1 2 3)) '(4 5 6)) '(6 5 4 1 2 3))

;; Exercise 22.3.1
;; Model:
;; build-number : (listof digit)  ->  number
;; to translate a list of digits into a number
;; example: (build-number (list 1 2 3)) = 123
(require htdp/gui)
(define (build-number digits)
  (local ((define (build-number-helper digits acc)
            (cond
              [(empty? digits) acc]
              [else (build-number-helper (rest digits) (+ (first digits) (* 10 acc)))])))
         (build-number-helper digits 0)))

;; View:
;; the ten digits as strings
(define DIGITS
  (build-list 10  number->string))

;; a list of three digit choice menus
(define digit-choosers
  (local ((define (builder i) (make-choice DIGITS)))
    (build-list 3 builder)))

;; a message field for saying hello and displaying the number
(define a-msg
  (make-message "Welcome"))

;; Controller:
;; check-call-back : X  ->  true
;; to get the current choices of digits, convert them to a number,
;; and to draw this number as a string into the message field
(define (check-call-back b)
  (draw-message a-msg
                (check-guess
                  (build-number
                    (map choice-index digit-choosers)))))

(define (check-guess guess)
  (if (= guess TARGET)
    "Bingo!"
    "Incorrect guess."))

(define TARGET 159)

(create-window
 (list
  (append digit-choosers (list a-msg))
  (list (make-button "Check Guess" check-call-back))))


;; Exercise 22.3.2
(require htdp/gui)
(define phone-numbers
  (list
    (list "Jingjing Duan" "2067909796")
    (list "Qingqing Lin" "2062342430")))

(define text-field (make-text "Enter name"))

(define a-message (make-message "ddd-ddd-dddd"))

(define (lookup-phone-number name table)
  (cond
    [(empty? table) "Not found"]
    [(string=? name (first (first table))) (second (first table))]
    [else (lookup-phone-number name (rest table))]))

(define (lookup-callback e)
  (draw-message a-message
                (lookup-phone-number (text-contents text-field)
                                     phone-numbers)))

(define a-button (make-button "lookup" lookup-callback))

(create-window
  (list
    (list text-field a-message)
    (list a-button)))

;; Exercise 22.3.3
(require htdp/gui)
(define (pad->gui title a-message table)
  (create-window
    (cons
      (list (make-message title))
      (cons
        (list a-message)
        (map list->buttons table)))))

(define a-message
  (make-message ""))

(define (button-callback e)
  (draw-message a-message
                "not working yet"))

(define (list->buttons lst)
  (map (lambda (cell)
               (cond
                 [(number? cell) (make-button (number->string cell) button-callback)]
                 [(symbol? cell) (make-button (symbol->string cell) button-callback)]
                 [else (error 'list->buttons "cell can only be number or symbol")]))
       lst))

(pad->gui "phone"
          a-message
          '((1 2 3)
              (4 5 6)
              (7 8 9)
              (\# 0 *)))
