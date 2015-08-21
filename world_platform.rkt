#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(require rackunit)

;; move a circle around the screen in response to arrow key presses

(struct posn (x y) #:transparent)
(struct worldstate (it1 it2) #:transparent)

(define DELTA 15)

(define RADIUS 20)

;; length of each side of the square
(define CANVAS-SIZE 400)

(define BACKGROUND (empty-scene CANVAS-SIZE CANVAS-SIZE))
(define IT (circle RADIUS "solid" "blue"))

;; adjust the xy point representing the center of the circle
;; so that it stays in the canvas
(define (adjust-to-canvas xy)
  (define (adjust-1d x)
    (cond
      [(< x RADIUS) RADIUS]
      [(> x (- CANVAS-SIZE RADIUS)) (- CANVAS-SIZE RADIUS)]
      [else x]))
  (posn (adjust-1d (posn-x xy))
        (adjust-1d (posn-y xy))))

(module+ test
  (check-equal? (adjust-to-canvas (posn 100 100))
                (posn 100 100))
  (check-equal? (adjust-to-canvas (posn 0 100))
                (posn RADIUS 100))
  (check-equal? (adjust-to-canvas (posn 100 0))
                (posn 100 RADIUS))
  (check-equal? (adjust-to-canvas (posn 0 0))
                (posn RADIUS RADIUS))
  (check-equal? (adjust-to-canvas (posn -10 200))
                (posn RADIUS 200))
  (check-equal? (adjust-to-canvas (posn 200 -10))
                (posn 200 RADIUS))
  (check-equal? (adjust-to-canvas (posn CANVAS-SIZE CANVAS-SIZE))
                (posn (- CANVAS-SIZE RADIUS) (- CANVAS-SIZE RADIUS)))
  )
                    
(define (key-handler pos key)
  (printf "key: ~v\n" key)
  (match key
    ["left" (adjust-to-canvas (struct-copy posn pos [x (- (posn-x pos) DELTA)]))]
    ["right" (adjust-to-canvas (struct-copy posn pos [x (+ (posn-x pos) DELTA)]))]
    ["up" (adjust-to-canvas (struct-copy posn pos [y (- (posn-y pos) DELTA)]))]
    ["down" (adjust-to-canvas (struct-copy posn pos [y (+ (posn-y pos) DELTA)]))]
    [_ pos]))

(define (wasd-to-arrows k)
  (match k
    ["w" "up"]
    ["a" "left"]
    ["s" "down"]
    ["d" "right"]
    [_ ""]))

(define (key-press ws key)
  (match-define (worldstate it1 it2) ws)
  (match key
    [(or "w" "a" "s" "d") #f]
    [(or "left" "right" "up" "down") #f]
    [_ ws]))

(define (render pos)
  (place-image IT (posn-x pos) (posn-y pos) BACKGROUND))

(define (main)
  (big-bang (posn RADIUS RADIUS)
            [on-key key-handler]
            [to-draw render]))

(main)
