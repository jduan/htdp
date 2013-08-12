;; Exercise 4.1.1
(and (> 4 3) (<= 10 100))   ;; => true
(or (> 4 3) (= 10 100))     ;; => true
(not (= 2 3))               ;; => true

;; Exercise 4.1.2
(> x 3)   ;; true when x = 4
(> x 3)   ;; false when x = 2
(> x 3)   ;; true when x = 7/2

(and (> 4 x) (> x 3)) ;; false when x = 4
(and (> 4 x) (> x 3)) ;; false when x = 2
(and (> 4 x) (> x 3)) ;; true when x = 7/2

(= (* x x) x) ;; false when x = 4
(= (* x x) x) ;; false when x = 2
(= (* x x) x) ;; false when x = 7/2
