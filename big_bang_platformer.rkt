#lang racket

(require 2htdp/universe
         2htdp/image
         (only-in "./geometry.rkt"
                    rect posn))

;; num * num * num
(struct motion (dx dy ddy) #:transparent)

;; rect * motion
(struct player (rect motion) #:transparent)

;; rect  * (posn -> (dx * dy))
(struct enemy (rect behavior) #:transparent)

;; player * (setof rect) * rect * (listof enemy)
(struct game-state (player env goal enemies) #:transparent)

;; game-state -> game-state
;; move the player
;; move the enemies
;; check if the player or enemies died
;; check if the player reached the goal
(define (timer-tick gs)
  gs)