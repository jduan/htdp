(list (list 'bob 0 'a)
      (list 'carl 1 'a)
      (list 'dana 2 'b)
      (list 'erik 3 'c))

;; Exercise 13.0.3
(equal? (list 0 1 2 3 4 5)
        (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
                                                      empty)))))))
(equal? (list (list 'adam 0) (list 'eve 1) (list 'louisXIV 2))
        (cons (cons 'adam (cons 0 empty))
              (cons (cons 'eve (cons 1 empty))
                    (cons (cons 'louisXIV (cons 2 empty))
                          empty))))
(equal? (list 1 (list 1 2) (list 1 2 3))
        (cons 1
              (cons (cons 1 (cons 2 empty))
                    (cons (cons 1 (cons 2 (cons 3 empty)))
                          empty))))

;; Exercise 13.0.4
(equal? (cons 'a (cons 'b (cons 'c (cons 'd (cons 'e empty)))))
        (list 'a 'b 'c 'd 'e))
(equal? (cons (cons 1 (cons 2 empty)) empty)
        (list (list 1 2)))
(equal? (cons 'a (cons (cons 1 empty) (cons false empty)))
        (list 'a (list 1) false))
(equal? (cons (cons 1 (cons 2 empty)) (cons (cons 2 (cons 3 empty)) empty))
        (list (list 1 2) (list 2 3)))

;; Exercise 13.0.5
(equal? (cons 'a (list 0 false))
        (cons 'a (cons 0 (cons false empty))))
(equal? (list (cons 1 (cons 13 empty)))
        (cons (cons 1 (cons 13 empty)) empty))
(equal? (list empty empty (cons 1 empty))
        (cons empty (cons empty (cons (cons 1 empty) empty))))
(equal? (cons 'a (cons (list 1) (list false empty)))
        (cons 'a (cons (cons 1 empty) (cons false (cons empty empty)))))

;; Exercise 13.0.6
(equal? (list (symbol=? 'a 'b) (symbol=? 'c 'c) false)
        (list false true false))
(equal? (list (+ 10 20) (* 10 20) (/ 10 20))
        (list 30 200 1/2))
(equal? (list 'dana 'jane 'mary 'laura)
        (cons 'dana (cons 'jane (cons 'mary (cons 'laura empty)))))

;; Exercise 13.0.7
(equal? (first (list 1 2 3))
        1)
(equal? (rest (list 1 2 3))
        (list 2 3))

;; Exercise 13.0.8
(equal? '(1 a 2 b 3 c)
        (list 1 'a 2 'b 3 'c))
(equal? '((alan 1000)
            (barb 2000)
            (carl 1500)
            (dawn 2300))
        (list (list 'alan 1000)
              (list 'barb 2000)
              (list 'carl 1500)
              (list 'dawn 2300)))
(equal? '((My First Paper)
            (Sean Fisler)
            (Section 1
                     (Subsection 1 Life is difficult)
                     (Subsection 2 But learning things makes it interesting))
            (Section 2
                     Conclusion? What conclusion?))
        (list (list 'My 'First 'Paper)
              (list 'Sean 'Fisler)
              (list 'Section 1
                    (list 'Subsection 1 'Life 'is 'difficult)
                    (list 'Subsection 2 'But 'learning 'things 'makes 'it 'interesting))
              (list 'Section 2 'Conclusion? 'What 'conclusion?)))
