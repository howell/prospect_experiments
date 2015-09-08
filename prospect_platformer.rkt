#lang prospect

;; every shape process registers with the world by asserting ('register label shape)
;; a shape can move by sending the message ('move label dx dy)
;; the game logic process listens for movements and asserts the actual locations
;; a rendering process listens for the actual locations and draws the map accordingly

(require "./geometry.rkt"
         rackunit)

;; velocity and acceleration
(struct motion (v a) #:transparent)

;; int
(struct move-x (dx) #:transparent)

;; int
(struct move-y (dy) #:transparent)

(struct jump () #:transparent)

(struct y-collision () #:transparent)

;; rect
(struct player (rect) #:transparent)

;; rect
(struct static (rect) #:transparent)

;; key
(struct key-press (key) #:transparent)

(struct timer-tick () #:transparent)

;; translate key presses into commands (messages)
;; left and right arrow keys become (move-x dx)
;; space becomes (jump)
(define (player-behavior e s)
  (define DX 1)
  (match e
    [(message (at-meta (key-press key)))
     (match key
       ["left" (transition s (message (move-x (- DX))))]
       ["right" (transition s (message (move-x DX)))]
       ["space" (transition s (message (jump)))]
       [_ #f])]
    [_ #f]))

(define (spawn-player)
  (spawn
   player-behavior
   (void)
   (sub (key-press ?) #:meta-level 1)))

;; the vertical motion behavior tries to move the player downward by sending (move-y dy)
;; this happens periodically when the timer sends a (timer-tick) message
;; when a (jump) message is received, temporarily move the player upward
;; when a (y-collision) is detected reset velocity to 0
;; state is a motion struct
(define (vertical-motion-behavior e s)
  (define JUMP-V 10)
  (match e
    [(message (jump))
     (if (zero? (motion-v s)) ;; TODO: better way to detect if this is a legal time to jump
         (transition (motion JUMP-V (motion-a s)) '())
         #f)]
    [(message (timer-tick))
     (define motion-n (motion (+ (motion-v s) (motion-a s)) (motion-a s)))
     (transition motion-n (list (message (move-y (motion-v s)))))]
    [(message (y-collision))
     (transition (motion 0 (motion-a s)) '())]
    [_ #f]))

;; the game logic process keeps track of the location of the player and the environment
;; it process move-x and move-y commands. When a collision along the y-axis occurs it
;; sends a (y-collision) message
;; sends a message with the location of the player every time it moves, (player rect)
(define (game-logic-behavior e s)
  #f)

;; draw the static objects defined by (static rect) assertions and update the screen
;; each time the player moves - (player rect) messages
(define (render-behavior e s)
  #f)

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
