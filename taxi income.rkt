;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname handin-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

#|
The City of Chicago regulates taxi fares, setting two kinds of rates:
there are meter fares for most pickups, and flat-rate fares for known
distances such as rides from hotels to the airport. The meter rates
are set as follows: The base fare for any ride is $3.05, and it costs
$1.80 per mile of travel.

How much does a 0.5 mi. ride cost? How about 1 mi.? 2 mi.?

Make a table that shows the fare for distances of 0.5  mi., 1 mi, 1.5
mi, 2 mi., 2.5 mi., 3 mi.

Create a formula for calculating fares from trip distances.

Use the formula to design a function that computes a taxi fare given
the distance traveled.
|#


#|
To supplement my meager teaching income, I shovel snow for some of my
neighbors. For shoveling a sidewalk and driveway, I charge each
neighbor $10 per job plus $5 per inch of snowfall to be shoveled.

How much do I get paid if I shovel for one neighbor after a storm that
drops 1 inch of snow? What if 4 neighbors hire me after a blizzard
puts down 14 inches?

Make a table that shows my income in terms of both inches of snow and
the number of neighbors that hire me. (The table should have at least
9 values.)

Create a formula for calculating how much I earn if I shovel d inches
of snow for each of n neighbors.

Use the formula to design a function that computes my snow shoveling
income given both the number of inches and number of neighbors.
|#











; process-taxi-fares :  ... -> ...
; Template for taxi-fares
#;
(define (process-taxi-fares distance ...)
  (cond
    [(string=? distance "0.5")    ...]
    [(string=? distance "1")  ...]
    [(string=? distance "1.5")  ...]
    [(string=? distance "2")  ...]
    [(string=? distance "2.5")  ...]
    [(string=? distance "3")  ...]))

; what-to-do : Distance -> Number
; Figures out what to do at an intersection.

(check-expect (what-to-do1 0.5)    3.95)
(check-expect (what-to-do1 1)  4.85)
(check-expect (what-to-do1 1.5)  5.75)
(check-expect (what-to-do1 2)  6.65)
(check-expect (what-to-do1 2.5)  7.55)
(check-expect (what-to-do1 3)  8.45)

;Strategy: struct. decomp.
(define (what-to-do1 d)
  (+ 3.05 (* d 1.8)))

(what-to-do1 0.5)
(what-to-do1 1)
(what-to-do1 1.5)
(what-to-do1 2)
(what-to-do1 2.5)
(what-to-do1 3)



; process-shovel-fares :  ... -> ...
; Template for shovel-fares
#;
(define (process-shovel-fares inch ...)
  (cond
    [(string=? inch "1")  ...]
    [(string=? inch "2")  ...]))


; what-to-do : Inch -> Number
; Figures out what to do at an intersection.

(check-expect (what-to-do 1 1)  15)
(check-expect (what-to-do 2 1)  20)
(check-expect (what-to-do 14 4) 110)
(check-expect (what-to-do 2 2)  30)
(check-expect (what-to-do 2 4)  50)
(check-expect (what-to-do 2 20)  210)
(check-expect (what-to-do 2 10)  110)
(check-expect (what-to-do 2 5)  60)

;Strategy: struct. decomp.
(define (what-to-do i n)
   (+ (* i 5)(* 10 n)))


(what-to-do 1 1)
(what-to-do 2 1)
(what-to-do 14 4)
(what-to-do 2 2)
(what-to-do 2 4)
(what-to-do 2 20)
(what-to-do 2 10)
(what-to-do 2 5)
(what-to-do 2 15)