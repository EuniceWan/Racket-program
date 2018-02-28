;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Do not write any recursive functions for this homework.

;; Use map to write this function:
;; build-straight-line : num (listof num) -> (listof posn)
;; returns a list of posns where the X coordinate is n and 
;; the Y coordinate is a number
;; in lon
;; e.g., (build-straight-line 2 (list 1 2 3)) 
;;       (list (make-posn 2 1) (make-posn 2 2) (make-posn 2 3))


(check-expect (build-straight-line 1 '())
              '())
(check-expect (build-straight-line 1 (list 4 7))
              (list (make-posn 1 4) (make-posn 1 7)))
(check-expect (build-straight-line 2 (list 1 2 3)) 
    (list (make-posn 2 1) (make-posn 2 2) (make-posn 2 3))) 


;;strategy: decision tree
(define (build-straight-line n lon)
  (cond
    [(empty? lon) '()]
    [else
     (map (lambda(number) (make-posn n number))
          lon)]))




;; Use filter to write this function:
;; pts-north : posn (listof posn) -> (listof posn)
;; returns the posns from lop that are north of p,
;; that is, whose y coordinate is greater than p's y coordinate

(check-expect (pts-north (make-posn 1 2) '())
              '())
(check-expect (pts-north (make-posn 1 2) (list (make-posn 2 4)))
              (list (make-posn 2 4)))
(check-expect (pts-north (make-posn 1 2) (list (make-posn 2 1)))
              '())
(check-expect (pts-north (make-posn 3 4)
                         (list (make-posn 1 4) (make-posn 4 7)))
              (list (make-posn 4 7)))

;;strategy: decision tree
(define (pts-north p lop)
   (cond
     [(empty? lop) '()]
     [else
      (filter (lambda (posn)
                (>  (posn-y posn) (posn-y p)))
              lop)]))



;; Use foldr to write this function:
;; total-width : (listof image) -> num
;; returns the sum of the widths of all images in loi
(check-expect (total-width  '()) 0)
(check-expect (total-width  (list (rectangle 10 30 "solid" "orange"))) 10)
(check-expect (total-width  (list (rectangle 10 30 "solid" "orange")
                                  (circle 30 "solid" "orange"))) 70)
;;strategy: decision tree
(define (total-width loi)
  (cond
     [(empty? loi) 0]
     [else
      (foldr  (lambda (x y)
                (+ (image-width x) y))
              0 loi)]))
           
    
    


  

;; Use map filter and foldr to write the next four functions.

;; The next exercises use functions to represent curves in the plane. A
;; curve can be represented as a function that accepts an x coordinate
;; and returns a y coordinate. For example, the straight line through the
;; origin can be represented as:
(define (diagonal x) x)


;; and a parabola sitting on the origin can be represented as 
(define (parabola x) (* x x))


(define points (list (make-posn 1 0) (make-posn 1 1) (make-posn 2 2)))

;; points-on-line : (num -> num) (listof posn) -> (listof posn)
;; return the points in pts that are on the curve described by f
;; e.g., (points-on-line diagonal points)
;;       (list (make-posn 1 1) (make-posn 2 2))
;; and   (points-on-line parabola points)
;;       (list (make-posn 1 1))
(check-expect (points-on-line diagonal  '()) '())
(check-expect (points-on-line diagonal points)
              (list (make-posn 1 1) (make-posn 2 2)))
(check-expect (points-on-line parabola points)
              (list (make-posn 1 1)))

;;strategy: dicision tree
(define (points-on-line f pts)
  (cond
     [(empty? pts) '()]
     [else
      (filter (lambda (posn)
                (equal? (posn-y posn) (f (posn-x posn))))                     
              pts)]))





;; positions: (num -> num) (listof num) -> (listof posn)
;; returns a list of positions on the curve `f' whose x-coordinates
;; are in lon
;; e.g., (positions parabola (list 1 2 3)) 
;;       (list (make-posn 1 1) (make-posn 2 4) (make-posn 3 9))

(check-expect (positions diagonal '()) '())
(check-expect (positions diagonal (list 1 2))
              (list (make-posn 1 1) (make-posn 2 2)))
(check-expect (positions parabola (list 1 2 3)) 
        (list (make-posn 1 1) (make-posn 2 4) (make-posn 3 9)))
        
;strategy:desicion tree
(define (positions f lon)
  (cond
    [(empty? lon) '()]
    [else
        (map (lambda (number)
                (make-posn number (f number))) 
             lon)]))
     

;; flatten-posns : (listof posn) -> (listof num)
;; constructs the list consisting of all the X and Y coordinates of each
;; of the posns in lop, in order.
;; e.g., (flatten-posns points)
;;       (list 1 0 1 1 2 2)

(check-expect (flatten-posns '()) '())
(check-expect (flatten-posns points)
             (list 1 0 1 1 2 2))


;strategy: function composition
(define (flatten-posns lop)
  (cond
    [(empty? lop) '()]  
    [else
     (foldr(lambda (x y r) (cons x (cons y r)))
           '() (position-x lop) (position-y lop))]))

(define (position-x x)
  (map (lambda (number)
         (posn-x number)) 
             x))
(define (position-y y)
  (map (lambda (number)
         (posn-y number)) 
             y))

;; possible-y-coords : (listof (num -> num)) num -> (listof num)
;; given a list of lines lof and an x-coordinate, returns the list
;; of what y-coordinate is associated with that x-coordinate in each curve
;; e.g., (possible-y-coords (list diagonal parabola) 7)
;;       (list 7 49)


(check-expect (possible-y-coords '() 1) '())
(check-expect (possible-y-coords (list parabola) 4)
       (list 16))
(check-expect (possible-y-coords (list diagonal parabola) 7)
       (list 7 49))

;strategy:desicion tree
(define (possible-y-coords lof x)
  (cond
     [(empty? lof) '()]
     [else
      (map (lambda (f)
                 (f x))
             lof)]))
    




