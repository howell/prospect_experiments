#lang racket

(require 2htdp/universe
         2htdp/image
         (only-in "./geometry.rkt"
                  rect rect-top-left
                  posn
                  move-rect
                  overlapping-rects?
                  point-distance)
         "./platform_lib.rkt")

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
  (match-define (game-state player0 env gl enemies0) gs)
  (match-define (player r p0-motion) player0)
  (match-define (motion dx dy ddy) p0-motion)
  (match-define (cons player1 y-col?) (move-player-y player0 dy env))
  (define enemies1
    (if (positive? dy)
        (filter (lambda (e) (not (overlapping-rects? player1 (enemy-rect e)))) enemies0)
        (enemies0)))
  (match-define (cons player2 x-col?) (move-player-x player1 dx env))
  gs)


