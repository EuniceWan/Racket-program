;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Here is an algorithm for sorting lists of numbers

  - if the list has 0 elements or 1 element, it is sorted; return it.

  - if the list has 2 or more elements, divide the list into two
    halves and recursively sort them. Note that you can divide the
    elements in half multiple ways; it does not need to be the first
    half and second half; it may even be easier to take the odd numbered
    and even-numbered elements.

|#

#|
  - combine the two sorted sublists into a single one by merging them

Here is an algorithm for merging the two lists:

  - if either list is empty, return the other one
  
  - if both are not empty, pick the list with the
    smaller first element and break it up into
    it's first element and the rest of the list.
    Recur with the entire list whose first element is
    larger and the rest of the list whose first element
    is smaller. Then cons the first element onto the
    result of the recursive call.

Design functions that implement this sorting algorithm.
For each function, write down if it is generative recursion
or structural recursion. Also write down the running
time of each function using Big Oh notation.



|#


     
;; structural recursion 

;; mergelist : [list of number] [list of number] -> [List of number]
;; Merge two given lists into one 
;; Running Time : O(n) - Linear time function 
;; example as test cases 
(check-expect (mergelist '() '()) '())
(check-expect (mergelist (list 3 7) '()) (list 3 7))
(check-expect (mergelist '() (list 4)) (list 4))
(check-expect (mergelist (list 3 6 8) (list 4)) (list 3 4 6 8))
(check-expect (mergelist (list 7 8 9) (list 4 6)) (list 4 6 7 8 9))


;; strategy: func.comp
(define (mergelist l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(> (first l1) (first l2))
     (splist (first l1) (rest l1) l2)]
    [else
     (splist (first l2) (rest l2) l1)]))

;; splist : Number [Listof Number] [Listof Number] -> [Listof Number]
;;strategy: func.comp 
(define (splist s l1 l2)
  (insert s (sortlist l1 l2)))


;; example as test cases
(check-expect (sortlist '() '()) '())
(check-expect (sortlist '() (list 2 5)) (list 2 5))
(check-expect (sortlist (list 0 5) '()) (list 0 5))
(check-expect (sortlist (list 3) (list 2 5)) (list 2 3 5))

;; sortlist : [list of number] [list of number] -> [List of number]
;; Sorts the two given lists into one list
;; Running Time : O(n) - Linear time function 
;; strategy: func.comp 
(define (sortlist l1 l2)
   (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (empty? l1) (not (empty? l2))) l2]
    [(and (empty? l2) (not (empty? l1))) l1]
    [else
     (cond
       [(< (first l1) (first l2))
        (cons (first l1) (sortlist (rest l1) l2))]
       [else
        (cons (first l2) (sortlist l1 (rest l2)))])]))

;; example
(check-expect (insert 4 (list 2 5)) (list 2 4 5))

;; insert : number [list of number] -> [List of number]
;; Insert the given number into the sorted list
;; Running Time : O(n) - Linear time function 
;;strategy: func.comp 
(define (insert s l)
  (cond
    [(< s (first l))
     (cons s l)]
    [else
     (cons (first l) (insert s (rest l)))])) 
     
;;template:
;;(define (fun-for-lon alon)
;;  (cond
;;    [(empty? alon) ...]
;;    [(equal? (length alon) 1)...]
;;    [else ... (first alon) ...
;;          ... (fun-for-lon (rest alon)) ...]))


;; a finallist is either:
;;  - '()
;;  - (cons number list-of-numbers)

;; finallist :  [list of number] -> [List of number]
;; finallist will give us the sorted list
;; 

;; example
(check-expect (finallist '()) '())
(check-expect (finallist (list 1)) (list 1))
(check-expect (finallist (list 7 2 5)) (list 2 5 7))

;; strategy: func.comp
(define (finallist l)
  (cond
    [(empty? l) '()]
    [(equal? (length l) 1) l]
    [else (mergelist (oddnumber l)
                     (evennumber l))]))

;; tamplate:
;;(define (list l)
;; (cond
;;    [(empty? l) '()]
;;    [else
;;        ...(first l)...
;;        ...(rest l)...  ]))


;; a oddnumber / evennumber is either:
;;  - '()
;;  - (cons number list-of-numbers)

;; oddnumber:  [list of number] -> [List of number]
;; sort list of odd numbers from small to large
;; Running Time : O(n) - Linear time function 

;;example as test cases
(check-expect (oddnumber '()) '())
(check-expect (evennumber (list 5)) '())
(check-expect (oddnumber (list 5 3)) (list 5))
(check-expect (oddnumber (list 3 5 2)) (list 2 3))
(check-expect (oddnumber (list 8 2 7 0)) (list 7 8))
(check-expect (evennumber (list 8 2)) (list 2))
(check-expect (evennumber (list 8 2 7 0)) (list 0 2))

;; strategy: struc. deco 
(define (oddnumber l)
  (cond
    [(empty? l)'()]
    [(or (empty? (rest l))
         (empty? (rest (rest l))))
     (cons (first l) '())]
    [else
     (sort (cons (first l) (oddnumber (rest (rest l)))) <)]))
    



;; evennumber :  [list of number] -> [List of number]
;; sort list of even numbers from small to large
;; Running Time : O(n) - Linear time function 
;; strategy: struc. deco
;; Example as test cases given above

(define (evennumber l)
  (cond
    [(or (empty? l)
         (empty? (rest l))) '()]
    [(empty? (rest (rest l)))
         (rest l) ]
    [else
     (sort (cons (second l) (evennumber (rest (rest l)))) <)]))
   


                 


  
