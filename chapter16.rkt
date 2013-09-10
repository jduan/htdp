;; Exercise 16.2.1
;; Translate the file system in figure 44 into a Scheme representation according
;; to model 1.
(define dir
  (list (list 'part1 'part2 'part3)
      (list (list 'hang 'draw)
            (list 'read!))
      'read!))

;; Exercise 16.2.2
;; Finds out the number of files in a given directory.
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(list? (first dir))
     (+ (how-many (first dir))
        (how-many (rest dir)))]
    [else (+ 1 (how-many (rest dir)))]))

(equal? (how-many dir) 7)

;; Exercise 16.2.3
;; Show how to model a directory with two more attributes: a size and a systems
;; attribute. The former measures how much space the directory itself (as
;; opposed to its files and subdirectories) consumes; the latter specifies
;; whether the directory is recognized by the operating system.
(define-struct dir (name content size systems))

;; Exercise 16.2.4
;; Translate the file system in figure 44 into a Scheme representation according
;; model 2.
(define text (make-dir 'Text (list 'part1 'part2 'part3) 10 true))
(define code (make-dir 'Code (list 'hang 'draw) 20 true))
(define docs (make-dir 'Docs (list 'read!) 30 true))
(define libs (make-dir 'Libs (list code docs) 40 true))
(define ts (make-dir 'TS (list text libs 'read!) 50 true))

;; Exercise 16.2.5
;; Develop the function how-many, which consumes a dir according to model 2 and
;; produces the number of files in the dir tree.
(define (how-many-content content)
  (cond
    [(empty? content) 0]
    [(symbol? (first content))
     (+ 1 (how-many-content (rest content)))]
    [else (+ (how-many (first content))
             (how-many-content (rest content)))]))
(define (how-many dir)
  (how-many-content (dir-content dir)))

(equal? (how-many ts) 7)

;; Exercise 16.3.1
(define-struct file (name size content))
(define-struct dir (name dirs files))

(define text (make-dir 'Text empty (list (make-file 'part1 99 empty)
                                         (make-file 'part2 52 empty)
                                         (make-file 'part3 17 empty))))
(define code (make-dir 'Code empty (list (make-file 'hang 8 empty)
                                         (make-file 'draw 2 empty))))
(define docs (make-dir 'Docs empty (list (make-file 'read! 19 empty))))
(define libs (make-dir 'Libs (list code docs) empty))
(define ts (make-dir 'TS (list text libs) (list (make-file 'read! 10 empty))))

;; Exercise 16.3.2
(define (how-many-in-dirs lod)
  (cond
    [(empty? lod) 0]
    [else (+ (how-many (first lod))
             (how-many-in-dirs (rest lod)))]))

(define (how-many dir)
  (+ (how-many-in-dirs (dir-dirs dir))
     (length (dir-files dir))))

(equal? (how-many ts) 7)

;; Exercise 16.3.3
;; Develop the function du-dir. The function consumes a directory and
;; computes the total size of all the files in the entire directory
;; tree.
(define (du-dir dir)
  (+ (du-in-dirs (dir-dirs dir))
     (du-in-files (dir-files dir))))

(define (du-in-dirs lod)
  (cond
    [(empty? lod) 0]
    [else (+ (du-dir (first lod))
             (du-in-dirs (rest lod)))]))

(define (du-in-files lof)
  (cond
    [(empty? lof) 0]
    [else (+ (file-size (first lof))
             (du-in-files (rest lof)))]))

(equal? (du-dir ts) 207)

;; Exercise 16.3.4
;; Develop the function find?, which consumes a dir and a file name and
;; determines whether or not a file with this name occurs in the directory
;; tree.
(define (find? dir filename)
  (or (find-in-dirs (dir-dirs dir) filename)
      (find-in-files (dir-files dir) filename)))

(define (find-in-dirs lod filename)
  (cond
    [(empty? lod) false]
    [else (or (find? (first lod) filename)
              (find-in-dirs (rest lod) filename))]))

(define (find-in-files lof filename)
  (cond
    [(empty? lof) false]
    [(symbol=? (file-name (first lof)) filename) true]
    [else (find-in-files (rest lof) filename)]))

(equal? (find? ts 'hang) true)
(equal? (find? ts 'draw) true)
(equal? (find? ts 'part1) true)
(equal? (find? ts 'part4) false)
