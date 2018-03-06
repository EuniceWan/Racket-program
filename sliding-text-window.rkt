;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname handin-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
                                                                                                                                                                       (require 2htdp/universe)
(require 2htdp/image)
#|

Write a universe program that will, piece by piece,
display the `complete-message` below. At any given
moment, we should see     3 characters of the
message and, as time ticks, the three characters
that are visible should move forward through the
message below. For example, at the first time tick,
we should see

  Wel

and the in the second time tick we should see:

  elc

and then

  lco

until time ticks to the end of the string. At that point,
we should see:

  IPD

and then

  PD!

and then the beginning of the string should appear, right
next to the content at the end, namely:

  D!W

and then

  !We

and then

  Wel

as before. And the entire process should repeat, for all time.

Try to make it be the case that if someone changes the
definition of `complete-message`, then your program should still
work (displaying the different message). If there are any situations
where your program would fail to work for some possible complete-messages,
note them in comments in your solution.

Think carefully about what the World state should be. It
needs to be some kind of a number, but not just any old number
is allowed. 

|#

(define complete-message "Welcome to IPD!")
;(define complete-message "Hah, Welcome to IPD!")
(define EMPTY (empty-scene 200 200))
;Data definition
(define-struct piece(i j))
;a Piece is (make-piece Number Number)

;Template for Piece
#;
(define (process-Piece p)
  ...(piece-i p)...
  ...(piece-j p)...)

;A worldstate is a Piece
(define WORLD0 (make-piece 0 3))

;change-text: WorldState->WorldState
;To change the start and end locations of current three letters to the start and end locations of next three letters
;Examples:
;(check-expect (make-piece 0 2) (make-piece 1 3))
;(check-expect (make-piece 12 14) (make-piece 13 0))
;Strategy: Struct.dec
(define (change-text p)
  (make-piece (remainder (+ 1 (remainder (piece-i p) (string-length complete-message))) (string-length complete-message))
              (remainder (+ 1 (remainder (piece-j p) (string-length complete-message))) (string-length complete-message))))

;tests only works for "Welcome to IPD!"
(check-expect (change-text (make-piece 0 2)) (make-piece 1 3 ))
(check-expect (change-text (make-piece 12 14)) (make-piece 13 0))

;draw-text: WorldState->Scene
;To draw the three letter in the current world to an empty scene
;Examples:

(define (draw-text p)
  (place-image (monospaced-text (co-substring p)) 100 100 EMPTY))

;co-substring: Piece->String
;To show the supposed three letter in the current world
;Examples:
;Strategy: Decision tree
(define (co-substring p)
  (cond
    [(< (piece-i p) (piece-j p)) (substring complete-message (piece-i p) (piece-j p))]
    [else (string-append (substring complete-message (piece-i p)) (substring complete-message 0 (piece-j p)))]))                             

;tests only works for "Welcome to IPD!"
(check-expect (co-substring (make-piece 1 3)) "el")
(check-expect (co-substring (make-piece 13 1)) "D!W")

#|
Note: when making the image, consider using a fixed-width font. The font
named “Courier New” or “Monospace” might work. (Usually most
computers have one of those installed.) Below is a helper function that
should do the trick on all platforms (but will look best on a mac)
|#

;; monspaced-text : string -> image
;; the body of this function uses something called a "symbol"
;; it is a lot like a string, except it begins with a ' mark
;; and ends with whitespace. Don't worry about this too much;
;; they are needed to call “text/font”
(define (monospaced-text str)
  (text/font str
             36
             "black"
             "Menlo" 'modern
             'normal 'normal #f))

(big-bang WORLD0
          (on-tick change-text 2)
          (on-draw draw-text))


