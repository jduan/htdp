(define-struct parent (children name date eyes))

(define Gustav (make-parent empty 'Gustav 1988 'brown))
(define Eva (make-parent (list Gustav) 'Eva 1965 'blue))
(define Fred (make-parent (list Gustav) 'Fred 1966 'pink))
(define Adam (make-parent empty 'Adam 1950 'yellow))
(define Dave (make-parent empty 'Dave 1955 'black))
(define Carl (make-parent (list Adam Dave Eva) 'Carl 1926 'green))
(define Bettina (make-parent (list Adam Dave Eva) 'Bettina 1926 'green))

(define (blue-eyed-children? children)
  (cond
    [(empty? children) false]
    [else (or (blue-eyed-descendant? (first children))
              (blue-eyed-children? (rest children)))]))

(define (blue-eyed-descendant? parent)
  (or
    (symbol=? 'blue (parent-eyes parent))
    (blue-eyed-children? (parent-children parent))))

(equal? false (blue-eyed-descendant? Gustav))
(equal? false (blue-eyed-descendant? Fred))
(equal? true (blue-eyed-descendant? Eva))
(equal? false (blue-eyed-descendant? Dave))
(equal? false (blue-eyed-descendant? Adam))
(equal? true (blue-eyed-descendant? Bettina))
(equal? true (blue-eyed-descendant? Carl))

;; Exercise 15.1.2
(define (how-far-removed-children children)
  (let* [(how-far (map how-far-removed children))
         (filtered (filter number? how-far))]
    (cond
      [(empty? filtered) false]
      [else (+ 1 (apply min filtered))])))

(define (how-far-removed parent)
  (cond
    [(symbol=? 'blue (parent-eyes parent)) 0]
    [else (how-far-removed-children (parent-children parent))]))

(equal? false (how-far-removed Gustav))
(equal? false (how-far-removed Fred))
(equal? 0 (how-far-removed Eva))
(equal? false (how-far-removed Dave))
(equal? false (how-far-removed Adam))
(equal? 1 (how-far-removed Bettina))
(equal? 1 (how-far-removed Carl))

;; Exercise 15.1.3
(define (count-descendants-children children)
  (cond
    [(empty? children) 0]
    [else (+ (count-descendants (first children))
             (count-descendants-children (rest children)))]))
(define (count-descendants parent)
  (+ 1 (count-descendants-children (parent-children parent))))

(equal? 1 (count-descendants Gustav))
(equal? 2 (count-descendants Fred))
(equal? 2 (count-descendants Eva))
(equal? 1 (count-descendants Dave))
(equal? 1 (count-descendants Adam))
(equal? 5 (count-descendants Bettina))
(equal? 5 (count-descendants Carl))

(define (count-proper-descendants parent)
  (count-descendants-children (parent-children parent)))

(equal? 0 (count-proper-descendants Gustav))
(equal? 1 (count-proper-descendants Fred))
(equal? 1 (count-proper-descendants Eva))
(equal? 0 (count-proper-descendants Dave))
(equal? 0 (count-proper-descendants Adam))
(equal? 4 (count-proper-descendants Bettina))
(equal? 4 (count-proper-descendants Carl))

;; Exercise 15.1.4
(define (eye-colors-children children)
  (cond
    [(empty? children) empty]
    [else (append (eye-colors (first children))
                  (eye-colors-children (rest children)))]))
(define (eye-colors parent)
  (cons (parent-eyes parent) (eye-colors-children (parent-children parent))))

(equal? '(green yellow black blue brown) (eye-colors Carl))
(equal? '(green yellow black blue brown) (eye-colors Bettina))
(equal? '(blue brown) (eye-colors Eva))

;; Exercise 15.3.1
(define-struct wp (header body))

(define (size-document document)
  (cond
    [(empty? document) 0]
    [(symbol? (first document)) (+ 1 (size-document (rest document)))]
    [(wp? (first document)) (+ (size-wp (first document))
                               (size-document (rest document)))]))
(define (size-wp wp)
  (size-document (wp-body wp)))

(define dogs-wp
  (make-wp 'dogs-wp
           (list 'my 'dogs 'web 'page)))

(define cats-wp
  (make-wp 'cats-wp
           (list 'my 'cats 'web 'page)))

(define my-wp
  (make-wp 'my-wp
           (list 'see 'my 'dog dogs-wp
                 'see 'my 'cat cats-wp)))

(equal? 4 (size-wp dogs-wp))
(equal? 4 (size-wp cats-wp))
(equal? 6 (size-wp my-wp))

;; Exercise 15.3.3
(define (occurs sym wp)
  (or (symbol=? sym (wp-header wp))
      (occurs-body sym (wp-body wp))))

(define (occurs-body sym document)
  (cond
    [(empty? document) false]
    [(symbol? (first document))
     (or (symbol=? (first document) sym)
         (occurs-body (rest document) sym))]
    [else
     (or (occurs sym (first document))
         (occurs-body sym (rest document)))]))

;; Exercise 15.3.4
(define (find-wrapper wp sym)
  (find wp sym empty))

(define (find wp sym path)
  (find-document (wp-body wp) (wp-header wp) sym path))

(define (find-document document header sym path)
  (cond
    [(empty? document) false]
    [(symbol? (first document))
     (cond
       [(symbol=? (first document) sym) (append path (list header))]
       [else (find-document (rest document) header sym path)])]
    [else (or (find (first document) sym (append path (list header)))
              (find-document (rest document) header sym path))]))

;; data examples:
(define empty-page (make-wp 'empty-page empty))
(define page-1-word (make-wp 'page-1-word (cons 'w1 empty)))
(define page-2-words (make-wp 'page-2-words (list 'w1 'w2)))
(define with-1-word-subpage (make-wp 'page-1-word-with-subpage (cons page-1-word empty)))
(define with-2-words-subpage (make-wp 'with-2-words-subpage (cons page-2-words empty)))
(define dense-page1 (make-wp 'realistic (list 'w3  page-2-words 'w4 page-1-word 'w5)))
(define dense-page2 (make-wp 'realistic (list 'w3  empty-page 'w4 with-1-word-subpage 'w5)))

(equal? (find-wrapper empty-page 'w1) false)
(equal? (find-wrapper page-1-word 'w1) (list 'page-1-word))
(equal? (find-wrapper page-2-words 'w3) false)
(equal? (find-wrapper with-2-words-subpage 'w2) (list 'with-2-words-subpage 'page-2-words))
(equal? (find-wrapper dense-page1 'no-in-there) false)
(equal? (find-wrapper dense-page1 'w1) (list 'realistic 'page-2-words))
(equal? (find-wrapper dense-page1 'w2) (list 'realistic 'page-2-words))
(equal? (find-wrapper dense-page2 'w1) (list 'realistic 'page-1-word-with-subpage 'page-1-word))
(equal? (find-wrapper dense-page1 'w5) (list 'realistic))
