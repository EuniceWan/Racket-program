;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

In Chicago, some traffic lights have, in addition to the
red/orange/green phases, a special pedestrian
crossing indication that changes to walk slightly before
the light turns green and an orange hand that appears
slightly before the main traffic light turns orange.

Design a data-type to capture all of the different
states that the light can take on. (Our solution
has five states; yours should probably too, but if
you think a different number makes more sense, go
for it.) When changing from one state to the next,
either the light color should change or the
walk/don’t-walk status should change, never both.

Write functions to support a big-bang program that
draws a traffic light that cycles appropriately.

|#
;;[enumeration]
; A traffic-light is one of:
;- "Pedestrain indication"
;- "green"
;- "orange hand"
;- "orange"
;- "red"

;A worldstate is a Number
(require 2htdp/image)
(require 2htdp/universe)

(define EMPTY       (empty-scene 200 200))
(define signal0
  (place-image (underlay/offset (text "signal 1" 12 "black") 40 45
                   (underlay/offset (circle 30 "solid" "green") 0 40 (text "Walk" 16 "green"))) 100 100 EMPTY))
(define signal1
  (place-image (underlay/offset (text "signal 2" 12 "black") 40 45
                   (underlay/offset (circle 30 "solid" "orange") 0 40 (text "Walk" 16 "green"))) 100 100 EMPTY))
(define signal2
  (place-image (underlay/offset (text "signal 3" 12 "black") 40 45
                   (underlay/offset (circle 30 "solid" "orange") 0 40 (text "Stop" 16 "red"))) 100 100 EMPTY))
(define signal3
  (place-image (underlay/offset (text "signal 4" 12 "black") 40 45
                   (underlay/offset (circle 30 "solid" "red") 0 40 (text "Stop" 16 "red"))) 100 100 EMPTY))
(define signal4
  (place-image (underlay/offset (text "signal 5" 12 "black") 40 45
                   (underlay/offset (circle 30 "solid" "green") 0 40 (text "stop" 16 "red"))) 100 100 EMPTY))
(define (traffic-lights s)
  (cond
   [(equal? (remainder s 5) 0) signal0]
   [(equal? (remainder s 5) 1) signal1]
   [(equal? (remainder s 5) 2) signal2]
   [(equal? (remainder s 5) 3) signal3]
   [(equal? (remainder s 5) 4) signal4]
   ))

(big-bang 0
          (on-tick add1 0.8)
          (to-draw traffic-lights))