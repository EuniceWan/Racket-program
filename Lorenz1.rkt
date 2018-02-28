;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lorenz1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

The Lorenz attractor is a beautiful (IMO) non-repeating motion in three
dimensional space that is governed by the following differential
equations (for appropriate definitions of the constants sigma, rho, and
beta):

dx/dt = sigma * (y-x)
dy/dt = x*(rho - z) - y
dz/dt = x*y - beta*z

We can simulate the motion that these equations describe by translating
them into BSL. (If you are not familiar with differential equations,
don’t worry; the assignment explains how to use them.)

|#
(require 2htdp/image)
(require 2htdp/universe)

(define sigma 10)
(define beta 8/3)
(define rho 28)

(define time-step #i0.01)
(define EMPTY       (empty-scene 600 600))

; Number Number Number -> Number
; Computes the next x coordinate given the x, y, and z coordinates.
;
; Strategy: domain knowledge
(define (next-x x y z)
  (+ x (* time-step (* sigma (- y x)))))

; Number Number Number -> Number
; Computes the next y coordinate given the x, y, and z coordinates.
;
; Strategy: domain knowledge
(define (next-y x y z)
  (+ y (* time-step (- (* x (- rho z)) y))))

; Number Number Number -> Number
; Computes the next z coordinate given the x, y, and z coordinates.
;
; Strategy: domain knowledge
(define (next-z x y z)
  (+ z (* time-step (- (* x y) (* beta z)))))

(check-within (next-x 1 1 1) 1 0.01)
(check-within (next-y 1 1 1) 1.26 0.01)
(check-within (next-z 1 1 1) 0.98 0.01)

(define-struct 3posn(x y z))
;a Point is (make-3posn Number Number Number)
;interp.the Point(x y z) in R^3.
;Template for Point
#;
(define (process-Point p...)
  ...(3posn-x p)...
  ...(3posn-y p)...
  ...(3posn-z p)...)

(define-struct 4point(p1 p2 p3 p4))
;a Chain is (make-4point Point Point Point Point)
;Template for Chain
#;
(define (process-Chain c...)
  ...(4point-p1 c)...
  ...(4point-p2 c)...
  ...(4point-p3 c)...
  ...(4point-p4 c)...)

;a world is a Chain
(define WORLD0 (make-4point (make-3posn 1 1 1)
                            (make-3posn (next-x 1 1 1)
                                        (next-y 1 1 1)
                                        (next-z 1 1 1))
                            (make-3posn (next-x (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1))
                                        (next-y (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1))
                                        (next-z (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)))
                            (make-3posn (next-x (next-x (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)) (next-y (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)) (next-z (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)))
                                        (next-y (next-x (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)) (next-y (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)) (next-z (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)))
                                        (next-z (next-x (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)) (next-y (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1)) (next-z (next-x 1 1 1) (next-y 1 1 1) (next-z 1 1 1))))
                            ))

;point-move:Point->Point
;figure out what is the next point's coordinate should be
;Examples:
;(check-within (point-move (make-3posn 1 1 1)) (make-3posn 1 1.26 0.98) 0.01)
;Strategy: struct.dec
(define (point-move p)
  (make-3posn (next-x (3posn-x p) (3posn-y p) (3posn-z p))
             (next-y (3posn-x p) (3posn-y p) (3posn-z p))
             (next-z (3posn-x p) (3posn-y p) (3posn-z p))))

(check-within (point-move (make-3posn 1 1 1)) (make-3posn 1 1.26 0.98) 0.01)

;chain-move:Worldstate->Worldstate
;figure out where is the next chain
;Examples:
;(check-within (chain-move (make-4point (make-3posn 1 1 1) (make-3posn 1 1 1) (make-3posn 1 1 1) (make-3posn 1 1 1))) (make-4point (make-3posn 1 1.26 0.98) (make-3posn 1 1.26 0.98) (make-3posn 1 1.26 0.98) (make-3posn 1 1.26 0.98)))

;Strategy:struct.dec
(define (chain-move c)
  (make-4point (point-move (4point-p1 c)) (point-move (4point-p2 c)) (point-move (4point-p3 c)) (point-move (4point-p4 c))))

;dragX:Point->Number
;drag the x coordinate in Point
;Examples:
(check-expect (dragX (make-3posn 1 1 2)) 310)
(check-expect (dragX (make-3posn 2 1 2)) 320)

(define (dragX p)
  (+ 300 (* 10 (3posn-x p))))

;dragY:Point->Number
;drag the y coordinate in Point
;Examples:
(check-expect (dragY (make-3posn 1 2 2)) 320)
(check-expect (dragY (make-3posn 2 1 2)) 310)

(define (dragY p)
  (+ 300 (* 10 (3posn-y p))))



;draw-chain:worldstate->Image
;draw the chain in current world to the scene(x-y)
;Examples:

(check-expect (draw-chain (make-4point (make-3posn 1 1 1) (make-3posn 1 1 1) (make-3posn 1 1 1) (make-3posn 1 1 1)))
                          (place-images
                           (list (circle 10 "solid" "black")
                                 (circle 10 "solid" "black")
                                 (circle 10 "solid" "black")
                                 (circle 10 "solid" "black"))
                           (list (make-posn 310 310)
                                 (make-posn 310 310)
                                 (make-posn 310 310)
                                 (make-posn 310 310))
                           EMPTY))
                          

(define (draw-chain c)
  (place-images
   (list (circle 10 "solid" "black")
         (circle 10 "solid" "black")
         (circle 10 "solid" "black")
         (circle 10 "solid" "black"))
   (list (make-posn (dragX (4point-p1 c)) (dragY (4point-p1 c)))
         (make-posn (dragX (4point-p2 c)) (dragY (4point-p2 c)))
         (make-posn (dragX (4point-p3 c)) (dragY (4point-p3 c)))
         (make-posn (dragX (4point-p4 c)) (dragY (4point-p4 c))))
   EMPTY))

;draw-chain-with-line12: WorldState->Image
(define (draw-chain-with-line12 c)
  (add-line (draw-chain c)
            (dragX (4point-p1 c)) (dragY (4point-p1 c))
            (dragX (4point-p2 c)) (dragY (4point-p2 c))
            "red")
  )

;draw-chain-with-line123: WorldState->Image
(define (draw-chain-with-line123 c)
  (add-line (draw-chain-with-line12 c)
            (dragX (4point-p2 c)) (dragY (4point-p2 c))
            (dragX (4point-p3 c)) (dragY (4point-p3 c))
            "red")
  )

;draw-chain-with-line1234: WorldState->Scene
(define (draw-chain-with-line1234 c)
  (add-line (draw-chain-with-line123 c)
            (dragX (4point-p3 c)) (dragY (4point-p3 c))
            (dragX (4point-p4 c)) (dragY (4point-p4 c))
            "red")
  )

(big-bang WORLD0
         (on-tick chain-move)
         (on-draw draw-chain-with-line1234)
 )





#|

The three functions next-x, next-y, and next-z compute the new positions
for the attractor given the old positions. That is, next-x consumes the
current (x,y,z) position and returns a new x coordinate. Ditto for
next-y and next-z.

Note that the “#i” in front of the time-step signifies that this is an
“inexact” number. See the documentation on check-within to help you
testing functions that use those “inexact” numbers. There is a lot to
say about these kinds of numbers; for now just now that there may be
some (usually slight) imprecision in these numbers.

Design a world structure that animates the motion of the Lorenz
attractor. The world structure should hold the three most recent points
that the attractor has visited.

Start by designing a structure for three dimensional points, much like
the one for posns, except with three coordinates. Include some examples
and be sure to include the initial point for the Lorenz simulation,
namely the point where x=1, y=1, and z=1.

To model the Lorenz attractor, you need to track the position in three
dimensional space, but to draw it, you need to map that two dimensions.
Do that by discarding the ‘z’ coordinate, multiplying the ‘x’ and ‘y’
coordinates by 10 and then adding 300 to them. Use an empty scene of
size 600x600 to draw the image.

The Wikipedia page (https://en.wikipedia.org/wiki/Lorenz_system, near
the top) has an animation of the Lorenz attractor. Your world program
should look something like the dot that moves around, except instead
of showing the entire picture in the background, it should show the
only the four most recent points, connected by three lines.

|#
