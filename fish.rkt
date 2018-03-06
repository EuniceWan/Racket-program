;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname handin-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Design an image that represents a (cartoon) animal
(but not a pig). Use definitions to factor out repeated
subparts of the image and to give names to individual
features.

Below is an example pig to give you some ideas.

|#
(require 2htdp/image)

(define eye
  (overlay (circle 3 "solid" "black")
           (circle 5 "solid" "blue")
           (circle 10 "solid" "white")))

(define top-fin
          (triangle/ass 60 30 30 "solid" "pink"))

(define bottom-fin
  (flip-vertical top-fin))


(define fishtail
  (rotate 25
          (triangle/ass 45 40 40 "solid" "pink")))

(define mouth
  (rotate 30
          (triangle/ass 60 10 10 "solid" "red")))

(define bubble
  (overlay/offset
   (overlay/offset(circle 8 "solid" "pink")
                -20 -20
                (circle 5 "solid" "pink"))            
   -25 -25
   (circle 3 "solid" "pink")))

(define fish-with-eye-and-tail
  (overlay/offset (beside
                   mouth
                   (rectangle 20 0 "solid" "pink")
                   eye
                   (rectangle 85 0 "solid" "pink")
                   fishtail)
                  -5 0
                  (ellipse 150 75 "solid" "pink")))

(define fish-with-eye-tail-and-fin
  (overlay
   (above top-fin
          (rectangle 0 50 "solid" "pink")
          bottom-fin)
   fish-with-eye-and-tail))

(define fish
         (overlay/offset
          bubble
          100 40
          fish-with-eye-tail-and-fin))

fish
#|
(require 2htdp/image)

(define eye
  (overlay (circle 3 "solid" "black")
           (circle 5 "solid" "blue")
           (circle 10 "solid" "white")))

(define right-ear
  (rotate 45
          (triangle/ass 90 60 40 "solid" "red")))

(define left-ear
  (flip-horizontal right-ear))

(define nose
  (overlay (beside (circle 3 "solid" "black")
                   (circle 3 "solid" "red")
                   (circle 3 "solid" "black"))
           (ellipse 50 30 "solid" "red")))

(define pig-with-eyes-and-nose
  (overlay (above (beside eye eye)
                  (circle 2 "solid" "pink")
                  nose)
           (circle 60 "solid" "pink")))

(define pig
  (overlay/align
   "center"
   "top"
   (beside left-ear
           (rectangle 60 0 "solid" "red")
           right-ear)
   pig-with-eyes-and-nose))

pig
|#