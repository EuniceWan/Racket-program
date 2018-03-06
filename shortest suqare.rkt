;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname handin-11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A (Listof X) is either:
;;  - '()
;;  - (cons number (Listof X))

;;Design the following functions:
;; squares : [Listof Number] -> [Listof Number]
;; compute the perfect squares of numbers in `l`

; Examples
(check-expect (squares '()) '())
(check-expect (squares (cons 1 '())) (cons 1 '()))
(check-expect (squares (cons 2 (cons 3 '()))) (cons 4 (cons 9 '())))

;Strategy: Structural decompostion 
(define (squares l)
  (cond
    [(empty? l) '()]
    [else (cons (* (first l) (first l)) (squares (rest l)))]))


;; contains-telephone : [listof String] -> Boolean
;; returns #true if 'l' contains the symbol 'telephone, #false otherwise

;Examples
(check-expect (contains-telephone '()) #false)
(check-expect (contains-telephone (cons "telephone" '())) #true)
(check-expect (contains-telephone (cons "name" (cons "telephone" '()))) #true)
(check-expect (contains-telephone (cons "name" '())) #false)

;Strategy: Structural decompostion 
(define (contains-telephone l)
  (cond
    [(empty? l)                          #false]
    [else
      (cond
        [(string=? "telephone" (first l) ) #true]
        [else   (contains-telephone (rest l))])]))

;; shortest : [NE-list-of String] -> String
;; returns the shortest string in 'l' -- read this for help:
;; http://www.ccs.neu.edu/home/matthias/HtDP2e/part_two.html#%28part._sec~3alists~3ane%29
;; also look up the 'string-length' function in the documentation (type f1 to open the docs)

; Examples
(check-expect (shortest '()) '())
(check-expect (shortest (cons "big" '())) "big")
(check-expect (shortest (cons "big" (cons "hard" '()))) "big")
(check-expect (shortest (cons "hard" (cons "big" '()))) "big")
(check-expect (shortest (cons "name"(cons "me" (cons "our" '())))) "me")

;Strategy: Structural decomposition
(define (shortest l)
  (cond
    [(not (empty? l))
     (cond
       [(empty? (rest l))              (first l)]
       [(< (string-length (first l)) (string-length (shortest (rest l))))   (first l)]
       [(> (string-length (first l)) (string-length (shortest (rest l))))   (shortest (rest l))])
     ]
    [else     '()]))
   
    
;; mean : [Listof Number] -> Number
;; computes the average of the elements of 'l'
;; return 0 if 'l' list is empty

;Examples
(check-expect (mean '()) '())
(check-expect (mean (cons 1 '())) 1)
(check-expect (mean (cons 1 (cons 3 '()))) 2)


;Strategy: Structural decomposition

(define (mean l)
  (cond
    [(not (empty? l))
     (cond
       [(empty? (rest l)) (first l)]
       [else (/ (+ (first l) (mean (rest l))) (length l))])
     ]
    [else    '()]))

