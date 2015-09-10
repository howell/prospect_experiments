#lang prospect

;; every shape process registers with the world by asserting ('register label shape)
;; a shape can move by sending the message ('move label dx dy)
;; the game logic process listens for movements and asserts the actual locations
;; a rendering process listens for the actual locations and draws the map accordingly

(require "./geometry.rkt"
         "./periodic_timer.rkt"
         rackunit
         prospect/drivers/timer
         racket/set
         racket/gui
         racket/draw)

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

(define static-detector (compile-projection (static (?!))))
(define static-rects-matcher
  (lambda (m)
    (set-map (matcher-project/set m static-detector) car)))

;; the game logic process keeps track of the location of the player and the environment
;; it process move-x and move-y commands. When a collision along the y-axis occurs it
;; sends a (y-collision) message
;; sends a message with the location of the player every time it moves, (player rect)
(define (game-logic-behavior e s)
  (match e
    [(message (move-x dx))
     (define player-n (car (move-player-x (game-state-player s) dx (game-state-env s))))
     (transition (game-state player-n (game-state-env s))
                 (list (message (player player-n))))]
    [(message (move-y dy))
     (match-define (cons player-n col?) (move-player-y (game-state-player s) dy (game-state-env s)))
     (transition (game-state player-n (game-state-env s))
                 (cons (message (player player-n))
                       (if (col?)
                           (list (message (y-collision)))
                           '())))]
    [(patch p-added p-removed)
     (define removed (static-rects-matcher p-removed))
     (define added (static-rects-matcher p-added))
     (define new-env (append added (remove* removed game-state-env s)))
     (transition (game-state (game-state-player s) new-env) '())]
    [_ #f]))

;; rect -> spawn
(define (spawn-game-logic player0)
  (spawn game-logic-behavior
         (game-state player0 '())
         (sub (move-x ?))
         (sub (move-y ?))
         (sub (static ?))
         (assert (player player0))))

;; num -> (U -1 0 1)
(define (my-sgn n)
  (cond
    [(zero? n) 0]
    [(negative? n) -1]
    [else 1]))

;; rect posn (listof rect) -> (U rect #f)
;; find the colliding rectange that is closest to the given position, if any exist
(define (closest-colliding r p rs)
  (define collisions (filter (lambda (x) (overlapping-rects? r x))
                             rs))
  (define sorted (sort collisions #:key rect-top-left
                       (lambda (p1 p2)
                         (< (point-distance p p1)
                            (point-distance p p2)))))
  (match sorted
    ['() #f]
    [(cons closest _) closest]))

(check-equal? (closest-colliding (rect (posn 0 0) 1 1)
                                 (posn 0 0)
                                 '())
              #f)

(check-equal? (closest-colliding (rect (posn 0 0) 2 1)
                                 (posn 0 0)
                                 (list (rect (posn 1 0) 1 1)))
              (rect (posn 1 0) 1 1))

(check-equal? (closest-colliding (rect (posn 1 1) 1 1)
                                 2
                                 (list (rect (posn 3 0) 2 30)))
              #f)

(check-equal? (closest-colliding (rect (posn 0 0) 10 1)
                                 (posn 0 0)
                                 (list (rect (posn 4 0) 1 1)
                                       (rect (posn 3 0) 1 1)
                                       (rect (posn 5 0) 1 1)))
              (rect (posn 3 0) 1 1))

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the x-axis
;; when a collision occurs move as far as possible without colliding
;; returns the new rect for the player as well as if a collision occured
(define (move-player-x p dx env)
  (match-define (rect (posn p-x0 p-y0) p-w p-h) p)
  (match-define (and p-n (rect (posn pn-x0 pn-y0) pn-w pn-h)) (move-rect p dx 0))
  (match-define motion-rect
    (if (negative? dx)
        (rect (posn pn-x0 pn-y0) (+ (abs dx) p-w) p-h)
        (rect (posn p-x0 p-y0) (+ dx p-w) p-h)))
  (define closest-col (closest-colliding motion-rect (rect-top-left p) env))
  (if closest-col
      (match-let* ([(rect (posn col-x0 _) col-w _) closest-col]
                   [new-x0 (if (< p-x0 col-x0)
                               (- col-x0 p-w)
                               (+ col-x0 (+ col-w p-w)))])
        (cons (rect (posn new-x0 p-y0) p-w p-h) #t))
      (cons p-n #f)))


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

(check-equal? (move-player-x (rect (posn 1 1) 1 1)
                             2
                             (list (rect (posn 3 0) 2 30)
                                   (rect (posn 40 40) 1 1)))
              (cons (rect (posn 2 1) 1 1) #t))

(check-equal? (move-player-x (rect (posn 0 0) 1 1)
                             10
                             (list (rect (posn 6 -8) 30 40)))
              (cons (rect (posn 5 0) 1 1) #t))

(check-equal? (move-player-x (rect (posn 0 0) 1 1)
                             10
                             (list (rect (posn 3 -4) 1 20)))
              (cons (rect (posn 2 0) 1 1) #t))

(check-equal? (move-player-x (rect (posn 0 0) 1 1)
                             4
                             (list (rect (posn 2 0) 1 1)
                                   (rect (posn 1 0) 1 1)))
              (cons (rect (posn 0 0) 1 1) #t))

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the y-axis
;; when a collision occurs move as far as possible without colliding
;; returns the new rect for the player as well as if a collision occured
(define (move-player-y p dy env)
  (match-define (rect (posn p-x0 p-y0) p-w p-h) p)
  (match-define (and p-n (rect (posn pn-x0 pn-y0) pn-w pn-h)) (move-rect p 0 dy))
  (match-define motion-rect
    (if (negative? dy)
        (rect (posn p-x0 p-y0) p-w (+ (abs dy) p-h))
        (rect (posn pn-x0 pn-y0) p-w (+ (abs dy) p-h))))
  (define closest-col (closest-colliding motion-rect (rect-top-left p) env))
  (if closest-col
      (match-let* ([(rect (posn _ col-y0) _ col-h) closest-col]
                   [new-x0 (if (< p-y0 col-y0)
                               (- col-y0 col-h p-h)
                               (+ col-y0 p-h))])
        (cons (rect (posn new-x0 p-y0) p-w p-h) #t))
      (cons p-n #f)))

;; drawing-context rect (listof rect) -> void
;; draws the game
(define (draw-game dc player env)
  (void))

;; draw the static objects defined by (static rect) assertions and update the screen
;; each time the player moves - (player rect) messages
;; state is the environment of static rects
(define ((render-behavior dc) e s)
  (match e
    [(patch p-added p-removed)
     (define added (static-rects-matcher p-added))
     (define removed (static-rects-matcher removed))
     (define player (matcher-project/set p-added (compile-projection (player (?!)))))
     (transition (append added (remove* removed s))
                 '())]
    [(

(define (spawn-renderer dc)
  #f)

;; gui stuff
(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (cond
        [(char? key-code) (key-handler (string key-code))]
        [(arrow? key-code) (key-handler (symbol->string key-code))]
        [else (void)]))
    (super-new)))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

(define (make-frame width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler (lambda (key) (send-ground-message (key-press key)))]))
    (send frame show #t)
    #;(define-values (x-max y-max) (send canvas get-client-size))
    #;(set-box! bot-right (posn x-max y-max))
    (define dc (send canvas get-dc))
    (spawn-renderer dc)))


#;(spawn-timer-driver)
#;(spawn-clock 1000/24)
