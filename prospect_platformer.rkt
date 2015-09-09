#lang prospect

;; every shape process registers with the world by asserting ('register label shape)
;; a shape can move by sending the message ('move label dx dy)
;; the game logic process listens for movements and asserts the actual locations
;; a rendering process listens for the actual locations and draws the map accordingly

(require "./geometry.rkt"
         "./periodic_timer.rkt"
         rackunit
         prospect/drivers/timer
         racket/set)

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

(define (spawn-vertical-motion gravity)
  (spawn vertical-motion-behavior
         (motion 0 gravity)
         (sub (jump))
         (sub (timer-tick))
         (sub (y-collision))))

;; create a clock that sends (timer-tick) every period-ms
(define (spawn-clock period-ms)
  (periodically period-ms (lambda () (message (timer-tick)))))

;; rect * (listof rect)
(struct game-state (player env) #:transparent)

;; the game logic process keeps track of the location of the player and the environment
;; it process move-x and move-y commands. When a collision along the y-axis occurs it
;; sends a (y-collision) message
;; sends a message with the location of the player every time it moves, (player rect)
(define game-logic-behavior
  (let* ([static-detector (compile-projection (static (?!)))]
         [matcher-static-rects (lambda (m)
                                 (set-map (matcher-project/set m static-detector) car))])
    (lambda (e s)
      (match e
        [(message (move-x dx))
         #f]
        [(message (move-y dy))
         #f]
        [(patch p-added p-removed)
         (define removed (matcher-static-rects p-removed))
         (define added (matcher-static-rects p-added))
         (define new-env (append added (remove* removed game-state-env s)))
         (transition (game-state (game-state-player s) new-env) '())]
        [_ #f]))))

;; rect -> spawn
(define (spawn-game-logic player0)
  (spawn game-logic-behavior
         (game-state player0 '())
         (sub (move-x ?))
         (sub (move-y ?))
         (sub (static ?))
         (assert (player player0))))

;; draw the static objects defined by (static rect) assertions and update the screen
;; each time the player moves - (player rect) messages
(define (render-behavior e s)
  #f)

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the x-axis
;; when a 
;; returns the new rect for the player as well as if a collision occured
(define (move-player-x p dx env)
  (cons p #f))

(check-equal? (move-player-x (rect (posn 0 0) 1 1)
                             1
                             '())
              (cons (rect (posn 1 0) 1 1) #f))

(check-equal? (move-player-x (rect (posn 0 0) 1 1)
                             1
                             (list (rect (posn 1 0) 1 1)))
              (cons (rect (posn 0 0) 1 1) #t))

(check-equal? (move-player-x (rect (posn 1 1) 1 1)
                             2
                             (list (rect (posn 3 0) 2 30)))
              (cons (rect (posn 2 1) 1 1) #t))

(spawn-timer-driver)
(spawn-clock 1000/24)
