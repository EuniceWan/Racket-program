;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |our falling-extension|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)

(define world-width 200)
(define world-height 300)
(define EMPTY  (empty-scene world-width world-height))
(define faller-radius 10)
(define paddle-height 12)
(define paddle-width 50)
(define faller-color "gold")
(define faller-color-point-gain "green")
(define faller-color-paddle-toggle "red")
(define paddle-color "blue")
(define score-posn-x 170)
(define score-posn-y 20)
(define faller-color-change-proportion (* world-height (/ 2 3)))
(define paddle-toggle-ll 1500)
(define paddle-toggle-ul 1800)

   
; Common Faller
(define (faller-common color)
  (circle faller-radius "solid" color))

; Basic image for faller
 (define faller-image (faller-common faller-color)) 

; Point gain faller image
(define faller-image-point-gain (faller-common faller-color-point-gain))

; Paddle Size Toggle faller image
(define faller-image-paddle-toggle (faller-common faller-color-paddle-toggle))


; Common Paddle
(define (paddle-common width)
  (rectangle width paddle-height "solid" paddle-color))

; Basic image for Paddle
(define paddle-image (paddle-common paddle-width))
 

; Wider Paddle
(define paddle-image-wider (paddle-common (* 2 paddle-width)))



#|

There are three separate pieces of the game
to be implemented: rendering the world
to an image to be shown on the screen, handling
user inputs, and adjusting the world as time passes.

Here are the sigantures of the three functions:

draw : World -> image

key : World Keypress -> World

tick : World -> World

draw and key are fairly well-described by the
description above; tick needs a little more
explanation. It has several, conceptually different
pieces:

1) move the fallers down the screen by one coordinate
2) if a faller touches the bottom of the screen,
   remove it from the world; if it overlaps with
   the paddle, also increment the score
3) possibly add a new faller to the list (starting
   at the top of the screen)
4) move the paddle

Be sure to design several different functions to
accomplish those tasks and not just one monolithic
function that does it all! (And think carefully about
how you might break up the other two functions, too.)

Don't forget: the coordinate system has the upper-most
y coordinate as 0 and increasing coordinates move
downwards on the screen. The left-most x coordinate
is 0 and increasing coordinates move to the right.

|#


#|
 use these two definitions in the design of your functions
 to know the width and height of the game's screen and the
 y location of the paddle
|#

#|

Use a definition like this one to (randomly) add a faller
to the current list. You may wish to adjust it based on
gameplay factors or the way you interpret posns as fallers.
Note that because of the randomness, this function is not
really testable with check-expect. It needs different
test suite support so we skip some tests.

|#
;; maybe-add-faller : [Listof posn] -> [Listof posn]
#;
(check-expect (maybe-add-faller
               (list (make-posn 0 0)
                     (make-posn 1 1)
                     (make-posn 2 2)
                     (make-posn 3 3)
                     (make-posn 4 4)
                     (make-posn 5 5)
                     (make-posn 6 6)
                     (make-posn 7 7)
                     (make-posn 8 8)
                     (make-posn 9 9)))
              (list (make-posn 0 0)
                    (make-posn 1 1)
                    (make-posn 2 2)
                    (make-posn 3 3)
                    (make-posn 4 4)
                    (make-posn 5 5)
                    (make-posn 6 6)
                    (make-posn 7 7)
                    (make-posn 8 8)
                    (make-posn 9 9)))

(define (maybe-add-faller fallers)
  (cond
    [(< (length fallers) 20)
     (cond
       [(zero? (random 20))
        (cons (make-posn (random world-width) 0)
              fallers)]
       [else fallers])]
    [else fallers]))
#|
|#

;; a list-of-posn is either
;;   - '()
;;   - (cons posn list-of-posn)

;; a direction is either
;;   - "left"
;;   - "right"


;; a World is
;;   (make-world number        -- x coordinate of the paddle
;;               direction     -- which way the paddle is moving
;;               list-of-posn  -- faller positions
;;               number        -- score
;;               number        -- to make the special faller for paddle shift 
;;               number(0 or 1)-- to make the paddle toggle between narrow and wide 
(define-struct world (paddle direction fallers score count toggle))
;(define-struct posn (x y))
;Template for posn
#;
(define (process-posn p)
  ...(posn-x p)...
  ...(posn-y p)...)

;add1-pointy: posn -> posn
;add 1 on point's y coordinate
;Strategy: struct.dec
(define (add1-pointy p)
  (make-posn (posn-x p) (add1 (posn-y p))))

(check-expect (add1-pointy (make-posn 2 3)) (make-posn 2 4))

;Template for fallers
#;
(define (process-fallers f)
  (cond
    [empty?...]
    [else
     ...(first f)...
     ...(process-fallers (rest f))...]))

;fallers-down: list-of-posn -> list-of-posn
;update the coordinates y of fallers to y+1
;Examples:
(check-expect (fallers-down '()) '())
(check-expect (fallers-down (cons (make-posn 1 2) '()))
                            (cons (make-posn 1 3) '()))
(check-expect (fallers-down (cons (make-posn 1 2) (cons (make-posn 6 10) '())))
                            (cons (make-posn 1 3) (cons (make-posn 6 11) '())))
;Strategy: struct.dec
(define (fallers-down fallers)
  (cond
    [(empty? fallers) empty]
    [else
     (cons (add1-pointy (first fallers))
           (fallers-down (rest fallers)))]))



;remove-fallers: world -> list-of-posn
;remove the fallers when fallers hit the paddle or the ground
; Stratergy : Structural Decomposition




(define (remove-fallers world)
  (cond
    [(empty? (world-fallers world)) '()]
    [else
     (cond
       [(or (equal? (posn-y (first (reverse (world-fallers world))))
                  (- world-height faller-radius))
            (and (equal? (posn-y (first (reverse (world-fallers world))))
                     (- world-height faller-radius paddle-height))
             (and (< (posn-x (first (reverse (world-fallers world))))
                     (+ (world-paddle world) (/ paddle-width 2)))
                  (> (posn-x (first (reverse (world-fallers world))))
                     (- (world-paddle world) (/ paddle-width 2)))
                  (= 0 (world-toggle world)))))
        (reverse (remove (first (reverse (world-fallers world)))
                         (remove-fallers (make-world (world-paddle world)
                                                     (world-direction world)
                                                     (rest(reverse (world-fallers world)))
                                                     (world-score world)
                                                     (world-count world)
                                                     (world-toggle world)))))]
        [(or (equal? (posn-y (first (reverse (world-fallers world))))
                  (- world-height faller-radius))
            (and (equal? (posn-y (first (reverse (world-fallers world))))
                     (- world-height faller-radius paddle-height))
             (and (< (posn-x (first (reverse (world-fallers world))))
                     (+ (world-paddle world) paddle-width))
                  (> (posn-x (first (reverse (world-fallers world))))
                     (- (world-paddle world) paddle-width))
                  (= 1 (world-toggle world)))))
        (reverse (remove (first (reverse (world-fallers world)))
                         (remove-fallers (make-world (world-paddle world)
                                         (world-direction world)
                                         (rest(reverse (world-fallers world)))
                                         (world-score world)
                                         (world-count world)
                                         (world-toggle world)))))]
       [else (world-fallers world)])]
))



;refresh-fallers: world -> list-of-posn
;the current world's fallers
;because of the randomness, this function is not really testable with check-expect.
; Stratergy : Function Composition
(define (refresh-fallers world)
  (maybe-add-faller (fallers-down (remove-fallers world))))


;catch-score: world -> world
;add 10 to score when catch and toggles the toggle variable accordingly  
; Stratergy : Domain Knowledge
; Example as tests
(check-expect (catch-score (make-world 100
                           "left"
                           (cons (make-posn 100 (- world-height paddle-height faller-radius)) '())
                           100 0 0))
                           (make-world 100
                           "left"
                           (cons (make-posn 100 (- world-height paddle-height faller-radius)) '())
                           110 0 0))

(check-expect (catch-score (make-world 100
                           "left"
                           (cons (make-posn 100 (- world-height paddle-height faller-radius)) '())
                           100 1600 0))
                           (make-world 100
                           "left"
                           (cons (make-posn 100 (- world-height paddle-height faller-radius)) '())
                           110 1600 1))

(check-expect (catch-score (make-world 100
                           "left"
                           (cons (make-posn 100 (- world-height paddle-height faller-radius)) '())
                           100 1600 1))
                           (make-world 100
                           "left"
                           (cons (make-posn 100 (- world-height paddle-height faller-radius)) '())
                           110 1600 0))



(define (catch-score world)
  (cond
    [(empty?( world-fallers world)) world]
    [else
     (cond
       [(and (equal? (posn-y (first (reverse (world-fallers world))))
                     (- world-height faller-radius paddle-height))
             (and (< (posn-x (first (reverse (world-fallers world))))
                     (+ (world-paddle world) paddle-width faller-radius))
                  (> (posn-x (first (reverse (world-fallers world))))
                     (- (world-paddle world) paddle-width faller-radius)))
             (= 1 (world-toggle world))
             (> (world-count world) paddle-toggle-ll)
          (< (world-count world) paddle-toggle-ul))
         (make-world (world-paddle world)
                                     (world-direction world)
                                     (world-fallers world)
                                     (+ 10 (world-score world))
                                     (world-count world)
                                     (toggle (world-toggle world)))]  
       
       [(and (equal? (posn-y (first (reverse (world-fallers world))))
                     (- world-height faller-radius paddle-height))
             (and (< (posn-x (first (reverse (world-fallers world))))
                     (+ (world-paddle world) (/ paddle-width 2) faller-radius))
                  (> (posn-x (first (reverse (world-fallers world))))
                     (- (world-paddle world) (/ paddle-width 2) faller-radius)))
             (= 0 (world-toggle world))
             (> (world-count world) paddle-toggle-ll)
          (< (world-count world) paddle-toggle-ul))
        (make-world (world-paddle world)
                                     (world-direction world)
                                     (world-fallers world)
                                     (+ 10 (world-score world))
                                     (world-count world)
                                     (toggle (world-toggle world)))]

       [(and (equal? (posn-y (first (reverse (world-fallers world))))
                     (- world-height faller-radius paddle-height))
             (and (< (posn-x (first (reverse (world-fallers world))))
                     (+ (world-paddle world) (/ paddle-width 2) faller-radius))
                  (> (posn-x (first (reverse (world-fallers world))))
                     (- (world-paddle world) (/ paddle-width 2) faller-radius)))
             (= 0 (world-toggle world)))
        (make-world (world-paddle world)
                                     (world-direction world)
                                     (world-fallers world)
                                     (+ 10 (world-score world))
                                     (world-count world)
                                     (world-toggle world))]

       [(and (equal? (posn-y (first (reverse (world-fallers world))))
                     (- world-height faller-radius paddle-height))
             (and (< (posn-x (first (reverse (world-fallers world))))
                     (+ (world-paddle world) paddle-width faller-radius))
                  (> (posn-x (first (reverse (world-fallers world))))
                     (- (world-paddle world) paddle-width faller-radius)))
             (= 1 (world-toggle world)))
         (make-world (world-paddle world)
                                     (world-direction world)
                                     (world-fallers world)
                                     (+ 10 (world-score world))
                                     (world-count world)
                                     (world-toggle world))]

       

       [else world])]))


;(posn-y (first (reverse (world-fallers world)))) keep for now

;key: World KeyEvt -> World
;to reverse the direction of paddle and score-1

(check-expect (key (make-world (/ world-width 2) "right" '() 1 0 0) " ")
                   (make-world (/ world-width 2) "left" '() 0 0 0))

;Strategy: Struct.dec
(define (key world ke)
  (cond
    [(string=? ke " ") (make-world (world-paddle world)
                                   (opposite (world-direction world))
                                   (world-fallers world)
                                   (- (world-score world) 1)
                                   (world-count world)
                                   (world-toggle world))]
    [else world]))

;opposite: String -> String
;make "right" to "left" and vice versa

(check-expect (opposite "right") "left")
(check-expect (opposite "left") "right")

;Strategy: Struct.dec
(define (opposite s)
  (cond
    [(string=? s "right") "left"]
    [else "right"]))


; toggle : Number -> Number
; Toggles 0 to 1 and 1 to 0
; Stratergy : Structural Decompostion 

; Example as test cases 

(check-expect (toggle 1) 0)
(check-expect (toggle 0) 1)

(define (toggle x)
  (cond
    [(= 0 x) 1]
    [else 0]))


;tick: World -> World
;change the worldstate as time goes by and increments the count vairable or
;makes it zero when it touches the upper limit
;Strategy: Struct.dec 


(define (tick world)
  (cond
    [(and (string=? "right" (world-direction world))
          (= 0 (world-toggle world)))
     (cond
       [(< (world-paddle world) (- world-width (/ paddle-width 2)))
        (make-world (+ 1 (world-paddle world))
                    (world-direction world)
                    (refresh-fallers world)
                    (world-score (catch-score world))
                    (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                    (world-toggle (catch-score world)))]
       [else (make-world (world-paddle world)
                         (opposite (world-direction world))
                         (refresh-fallers world)
                         (world-score (catch-score world))
                         (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                         (world-toggle (catch-score world)))])]

    [(and (string=? "right" (world-direction world))
          (= 1 (world-toggle world)))
     (cond
       [(< (world-paddle world) (- world-width paddle-width))
        (make-world (+ 1 (world-paddle world))
                    (world-direction world)
                    (refresh-fallers world)
                    (world-score (catch-score world))
                    (cond
                      [(= (world-count world) paddle-toggle-ul) 0] 
                      [else (+ 1 (world-count world))])
                    (world-toggle (catch-score world)))]
       [else (make-world (world-paddle world)
                         (opposite (world-direction world))
                         (refresh-fallers world)
                         (world-score (catch-score world))
                         (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                         (world-toggle (catch-score world)))])]

    [(and (string=? "left" (world-direction world))
          (= 0 (world-toggle world)))
     (cond
       [(> (world-paddle world) (/ paddle-width 2))
        (make-world (- (world-paddle world) 1)
                    (world-direction world)
                    (refresh-fallers world)
                    (world-score (catch-score world))
                    (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                    (world-toggle (catch-score world)))]
       [else (make-world (world-paddle world)
                         (opposite (world-direction world))
                         (refresh-fallers world)
                         (world-score (catch-score world))
                        (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                         (world-toggle (catch-score world)))])]
    
    [(and (string=? "left" (world-direction world))
          (= 1 (world-toggle world)))
     (cond
       [(> (world-paddle world) paddle-width)
        (make-world (- (world-paddle world) 1)
                    (world-direction world)
                    (refresh-fallers world)
                    (world-score (catch-score world))
                    (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                    (world-toggle (catch-score world)))]
       [else (make-world (world-paddle world)
                         (opposite (world-direction world))
                         (refresh-fallers world)
                         (world-score (catch-score world))
                         (cond
                      [(= (world-count world) paddle-toggle-ul) 0]
                      [else (+ 1 (world-count world))])
                         (world-toggle (catch-score world)))])]))

;draw1: world -> image
;to draw the current world
; Stratergy : Domain Knowledge 
(define (draw1 world)
  (cond
    [(= 1 (world-toggle world))
                               (place-image paddle-image-wider
                               (world-paddle world)
                               (- world-height (/ paddle-height 2))
                               EMPTY)]
    [else (place-image paddle-image
          (world-paddle world)
          (- world-height (/ paddle-height 2))
          EMPTY)]))

;draw2: world -> image
;draw fallers on image from draw1 - and draw the paddle width toggling faller
;and the faller which would help notifying the user that it'll gain points. 
; Stratergy : Structural Decomposition 

(define (draw2 world)
  (cond
    [(equal? (world-fallers world) '()) (draw1 world)]

    

    [(and (> (world-count world) paddle-toggle-ll)
          (< (world-count world) paddle-toggle-ul))
                                             (place-image faller-image-paddle-toggle
                  (posn-x (first (world-fallers world)))
                  (posn-y (first (world-fallers world)))
                  (draw2 (make-world (world-paddle world)
                                     (world-direction world)
                                     (rest (world-fallers world))
                                     (world-score world)
                                     (world-count world)
                                     (world-toggle world))))]
  
    

     [(and (> (posn-y (first (world-fallers world)))
              faller-color-change-proportion)
          (< (posn-x (first (world-fallers world)))
             (+ (world-paddle world) (/ paddle-width 2) faller-radius))
          (> (posn-x (first (world-fallers world)))
             (- (world-paddle world) (/ paddle-width 2) faller-radius))
          (= 0 (world-toggle world)))
     (place-image faller-image-point-gain
                  (posn-x (first (world-fallers world)))
                  (posn-y (first (world-fallers world)))
                  (draw2 (make-world (world-paddle world)
                                     (world-direction world)
                                     (rest (world-fallers world))
                                     (world-score world)
                                     (world-count world)
                                     (world-toggle world))))]

    

     [(and (> (posn-y (first (world-fallers world)))
              faller-color-change-proportion)
          (< (posn-x (first (world-fallers world)))
             (+ (world-paddle world)  paddle-width  faller-radius))
          (> (posn-x (first (world-fallers world)))
             (- (world-paddle world)  paddle-width  faller-radius))
          (= 1 (world-toggle world)))
     (place-image faller-image-point-gain
                  (posn-x (first (world-fallers world)))
                  (posn-y (first (world-fallers world)))
                  (draw2 (make-world (world-paddle world)
                                     (world-direction world)
                                     (rest (world-fallers world))
                                     (world-score world)
                                     (world-count world)
                                     (world-toggle world))))]
    
    
    [else
     (place-image faller-image
                  (posn-x (first (world-fallers world)))
                  (posn-y (first (world-fallers world)))
                  (draw2 (make-world (world-paddle world)
                                     (world-direction world)
                                     (rest (world-fallers world))
                                     (world-score world)
                                     (world-count world)
                                     (world-toggle world))))]))

;draw3: world -> Image
;Display the score or the you lose message. 
; Stratergy : Domain Knowledge




(define (draw3 world)
  (cond
    [(< (world-score world) 0)
     (place-image (text "YOU LOSE" 20 "black")
                  (/ world-width 2)
                  (/ world-height 2)
                  (draw2 world))]
    
    [else (place-image (text (number->string (world-score world)) 20 "black")
               score-posn-x
               score-posn-y
               (draw2 world))]))
;world -> boolean
(define (world-stop world)
  (< (world-score world) 0))

(big-bang (make-world (/ world-width 2) "right" '() 0 0 1)
          [on-tick tick 1/200]
          [on-key key]
          [to-draw draw3]
          [stop-when world-stop])
#|

To run your game, do this:
|#    


