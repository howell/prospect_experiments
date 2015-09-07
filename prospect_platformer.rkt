#lang prospect

;; every shape process registers with the world by asserting ('register label shape)
;; a shape can move by sending the message ('move label dx dy)
;; the game logic process listens for movements and asserts the actual locations
;; a rendering process listens for the actual locations and draws the map accordingly

(require "./geometry.rkt"
         rackunit)

(struct rect (top-left width height) #:transparent)

(struct motion (x y dx dy ddx ddy))

;; rect motion [listof rect] -> rect
;;
(define (move r m lor)
  r)

(check-equal? (move (rect (posn 0 0) 1 1)
                    (motion 1 1 0 0 0 0)
                    '())
              (rect (posn 1 1) 1 1))

(check-equal? (move (rect (posn 0 0) 1 1)
                    (motion 1 0 0 0 0 0)
                    (list (rect (posn 1 0) 1 1)))
              (rect (posn 0 0) 1 1))
