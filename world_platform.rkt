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
(define IT1 (circle RADIUS "solid" "blue"))
(define IT2 (circle RADIUS "solid" "red"))

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
                (posn (- CANVAS-SIZE RADIUS) (- CANVAS-SIZE RADIUS))))

(define (move-in-canvas pos key)
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
    [_ #f]))

(module+ test
  (check-equal? (wasd-to-arrows "w")
                "up")
  (check-equal? (wasd-to-arrows "a")
                "left")
  (check-equal? (wasd-to-arrows "s")
                "down")
  (check-equal? (wasd-to-arrows "d")
                "right")
  (check-equal? (wasd-to-arrows "left")
                #f)
  (check-equal? (wasd-to-arrows "right")
                #f)
  (check-equal? (wasd-to-arrows "k")
                #f))

(define (key-press ws key)
  (match-define (worldstate it1 it2) ws)
  (match key
    [(or "w" "a" "s" "d")
     (define it2-n (move-in-canvas it2 (wasd-to-arrows key)))
     (struct-copy worldstate ws [it2 it2-n])]
    [(or "left" "right" "up" "down")
     (define it1-n (move-in-canvas it1 key))
     (struct-copy worldstate ws [it1 it1-n])]
    [_ ws]))

;; check if two circles with radius r at positions p1 and p2
;; are colliding
(define (colliding-circles? p1 r1 p2 r2)
  (match-define (posn x1 y1) p1)
  (match-define (posn x2 y2) p2)
  (define d^2 (+ (expt (- x1 x2) 2)
                 (expt (- y1 y2) 2)))
  (define r^2 (expt (+ r1 r2) 2))
  (< d^2 r^2))

(module+ test
  (check-true (colliding-circles? (posn 0 0)
                                  (posn 0 0)
                                  1))
  (check-true (colliding-circles? (posn 0 0)
                                  (posn 1 1)
                                  1)))

(define (render ws)
  (match-define (worldstate it1 it2) ws)
  (place-image IT2 (posn-x it2) (posn-y it2)
               (place-image IT1 (posn-x it1) (posn-y it1) BACKGROUND)))

(define (main)
  (big-bang (worldstate
             (posn RADIUS RADIUS)
             (posn (- CANVAS-SIZE RADIUS) (- CANVAS-SIZE RADIUS)))
            [on-key key-press]
            [to-draw render]))

#;(main)
