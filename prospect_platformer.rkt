#lang prospect

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

(struct move-left () #:transparent)
(struct move-right () #:transparent)

(struct jump () #:transparent)

(struct y-collision () #:transparent)

;; rect
(struct player (rect) #:transparent)

;; rect
(struct goal (rect) #:transparent)

;; rect
(struct static (rect) #:transparent)

;; key
(struct key-press (key) #:transparent)
(struct key-release (key) #:transparent)

(struct timer-tick () #:transparent)

;; translate key presses into commands
;; asserts (move-left)/(move-right) while the left/right arrow key is held down
;; space becomes a (jump) message
(define (player-behavior e s)
  ;; state is (U 'left 'right #f)
  (match e
    [(message (at-meta (key-press key)))
     (match key
       ['left (if s
                  #f
                  (transition key (assert (move-left))))]
       ['right (if s
                  #f
                  (transition key (assert (move-right))))]
       [#\space (transition s (message (jump)))]
       [_ #f])]
    [(message (at-meta (key-release (== s))))
     (transition #f
                 (list (retract (move-left))
                       (retract (move-right))))]
    [_ #f]))

(define (spawn-player)
  (spawn
   player-behavior
   #f
   (sub (key-press ?) #:meta-level 1)
   (sub (key-release ?) #:meta-level 1)))

(define left-matcher (compile-projection (move-left)))
(define right-matcher (compile-projection (move-right)))
(define (not-set-empty? s) (not (set-empty? s)))

;; the horizontal motion behavior tries to move the player along the x-axis
;; by sending the messsage (move-x +-dx) each timer tick while (move-left) or
;; (move-right) is being asserted
(define ((horizontal-motion-behavior dx) e s)
  ;; state is (U #f 'left 'right)
  (match e
    [(patch p-added p-removed)
     (define left-added? (not-set-empty? (matcher-project/set p-added left-matcher)))
     (define left-removed? (not-set-empty? (matcher-project/set p-removed left-matcher)))
     (define right-added? (not-set-empty? (matcher-project/set p-added right-matcher)))
     (define right-removed? (not-set-empty? (matcher-project/set p-removed right-matcher)))
     (cond
       [left-added? (transition 'left '())]
       [right-added? (transition 'right '())]
       [(or left-removed? right-removed?) (transition #f '())]
       [else #f])]
    [(message (timer-tick))
     (match s
       ['left (transition s (message (move-x (- dx))))]
       ['right (transition s (message (move-x dx)))]
       [_ #f])]
    [_ #f]))

(define (spawn-horizontal-motion dx)
  (spawn (horizontal-motion-behavior dx)
         #f
         (sub (move-left))
         (sub (move-right))
         (sub (timer-tick))))

;; the vertical motion behavior tries to move the player downward by sending (move-y dy)
;; this happens periodically when the timer sends a (timer-tick) message
;; when a (jump) message is received, temporarily move the player upward
;; when a (y-collision) is detected reset velocity to 0
;; state is a motion struct
(define ((vertical-motion-behavior jump-v v-max) e s)
  (match e
    [(message (jump))
     (if (< (motion-v s) .2) ;; TODO: better way to detect if this is a legal time to jump
         (transition (motion jump-v (motion-a s)) '())
         #f)]
    [(message (timer-tick))
     (define motion-n (motion (min v-max (+ (motion-v s) (motion-a s))) (motion-a s)))
     (transition motion-n (list (message (move-y (motion-v s)))))]
    [(message (y-collision))
     (transition (motion 0 (motion-a s)) '())]
    [_ #f]))

(define (spawn-vertical-motion gravity jump-v max-v)
  (spawn (vertical-motion-behavior jump-v max-v)
         (motion 0 gravity)
         (sub (jump))
         (sub (timer-tick))
         (sub (y-collision))))

;; create a clock that sends (timer-tick) every period-ms
(define (spawn-clock period-ms)
  (periodically period-ms (lambda () (message (timer-tick)))))

;; rect * (listof rect) * rect
(struct game-state (player env goal) #:transparent)

(define static-detector (compile-projection (static (?!))))
(define static-rects-matcher
  (lambda (m)
    (set-map (matcher-project/set m static-detector) car)))

;; the game logic process keeps track of the location of the player and the environment
;; it process move-x and move-y commands. When a collision along the y-axis occurs it
;; sends a (y-collision) message
;; sends a message with the location of the player every time it moves, (player rect)
;; asserts the location of the goal as (goal g)
(define (game-logic-behavior e s)
  (match e
    [(message (move-x dx))
     (define player-n (car (move-player-x (game-state-player s) dx (game-state-env s))))
     (transition (game-state player-n (game-state-env s) (game-state-goal s))
                 (list (message (player player-n))))]
    [(message (move-y dy))
     (match-define (cons player-n col?) (move-player-y (game-state-player s) dy (game-state-env s)))
     (transition (game-state player-n (game-state-env s) (game-state-goal s))
                 (cons (message (player player-n))
                       (if col?
                           (list (message (y-collision)))
                           '())))]
    [(patch p-added p-removed)
     (define removed (static-rects-matcher p-removed))
     (define added (static-rects-matcher p-added))
     (define new-env (append added (remove* removed (game-state-env s))))
     (transition (game-state (game-state-player s) new-env (game-state-goal s)) '())]
    [_ #f]))

;; rect goal -> spawn
(define (spawn-game-logic player0 goal0)
  (spawn game-logic-behavior
         (game-state player0 '() goal0)
         (sub (move-x ?))
         (sub (move-y ?))
         (sub (static ?))
         (assert (player player0))
         (assert goal0)))

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
                               (+ col-x0 col-w))])
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

(check-equal? (move-player-x (rect (posn 1 0) 1 1)
                             -1
                             (list (rect (posn 0 0) 1 1)))
              (cons (rect (posn 1 0) 1 1) #t))

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the y-axis
;; when a collision occurs move as far as possible without colliding
;; returns the new rect for the player as well as if a collision occured
(define (move-player-y p dy env)
  (match-define (rect (posn p-x0 p-y0) p-w p-h) p)
  (match-define (and p-n (rect (posn pn-x0 pn-y0) pn-w pn-h)) (move-rect p 0 dy))
  (match-define motion-rect
    (if (negative? dy)
        (rect (posn pn-x0 pn-y0) p-w (+ (abs dy) p-h))
        (rect (posn p-x0 p-y0) p-w (+ (abs dy) p-h))))
  (define closest-col (closest-colliding motion-rect (rect-top-left p) env))
  (if closest-col
      (match-let* ([(rect (posn _ col-y0) _ col-h) closest-col]
                   [new-y0 (if (< p-y0 col-y0)
                               (- col-y0 p-h)
                               (+ col-y0 col-h))])
        (cons (rect (posn p-x0 new-y0) p-w p-h) #t))
      (cons p-n #f)))

(check-equal? (move-player-y (rect (posn 0 0) 1 1)
                             1
                             '())
              (cons (rect (posn 0 1) 1 1) #f))

(check-equal? (move-player-y (rect (posn 0 0) 1 1)
                             1
                             (list (rect (posn 0 1) 1 1)))
              (cons (rect (posn 0 0) 1 1) #t))

(check-equal? (move-player-y (rect (posn 1 1) 1 1)
                             2
                             (list (rect (posn 0 3) 30 2)))
              (cons (rect (posn 1 2) 1 1) #t))

(check-equal? (move-player-y (rect (posn 1 1) 1 1)
                             2
                             (list (rect (posn 0 3) 30 2)
                                   (rect (posn 40 40) 1 1)))
              (cons (rect (posn 1 2) 1 1) #t))

(check-equal? (move-player-y (rect (posn 0 0) 1 1)
                             10
                             (list (rect (posn -8 6) 40 30)))
              (cons (rect (posn 0 5) 1 1) #t))

(check-equal? (move-player-y (rect (posn 0 0) 1 1)
                             10
                             (list (rect (posn -4 3) 20 1)))
              (cons (rect (posn 0 2) 1 1) #t))

(check-equal? (move-player-y (rect (posn 0 0) 1 1)
                             4
                             (list (rect (posn 0 2) 1 1)
                                   (rect (posn 0 1) 1 1)))
              (cons (rect (posn 0 0) 1 1) #t))

(define (star-points scl)
  (map (lambda (pr) (cons (* scl (car pr)) (* scl (cdr pr))))
  `((0 . 10)
    (2 . 6)
    (0 . 4)
    (3 . 4)
    (5 . 0)
    (7 . 4)
    (10 . 4)
    (8 . 6)
    (10 . 10)
    (5 . 7))))

;; drawing-context goal -> void
;; draws the goal as a 3x3 yellow star
(define (draw-goal dc g)
  (match-define (goal (rect (posn x0 y0) _ _)) g)
  (send dc set-brush "yellow" 'solid)
  (send dc set-pen "yellow" 1 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (star-points 5) x0 y0))

;; drawing-context rect color -> void
;; draws a black rectangle
(define (draw-rect dc r color)
  (match-define (rect (posn x0 y0) w h) r)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-rectangle x0 y0 w h))

;; drawing-context rect (listof rect) goal -> void
;; draws the game
(define (draw-game dc player env gl)
  (send dc suspend-flush)
  (send dc clear)
  (for ([r env])
    (draw-rect dc r "black"))
  (draw-goal dc gl)
  (draw-rect dc player "blue")
  (send dc resume-flush))

;; draw the static objects defined by (static rect) and (goal rect) assertions and update the screen
;; each time the player moves - (player rect) messages
;; state is a game-state struct
(define ((render-behavior dc) e s)
  (match-define (game-state old-player old-env old-goal) s)
  (match e
    [(patch p-added p-removed)
     (define added (static-rects-matcher p-added))
     (define removed (static-rects-matcher p-removed))
     (define new-env (append added (remove* removed old-env)))
     (define player-s (matcher-project/set p-added (compile-projection (player (?!)))))
     (define new-player (if (set-empty? player-s) old-player (car (set-first player-s))))
     (define goal-s (matcher-project/set p-added (compile-projection (goal (?!)))))
     (define new-goal (if (set-empty? goal-s) old-goal (goal (car (set-first goal-s)))))
     (draw-game dc new-player new-env new-goal)
     (transition (game-state new-player new-env new-goal)
                 '())]
    [(message (player new-player))
     (draw-game dc new-player old-env old-goal)
     (transition (game-state new-player old-env old-goal)
                 '())]
    [_ #f]))

(define (spawn-renderer dc)
  (spawn
   (render-behavior dc)
   (game-state (rect (posn 0 0) 0 0) '() (goal (rect (posn -100 -100) 0 0)) )
   (sub (static ?))
   (sub (player ?))
   (sub (goal ?))))

;; gui stuff
(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (define release-code (send event get-key-release-code))
      #;(printf "~v\n" key-code)
      (cond
        [(release? key-code) (key-handler (key-release release-code))]
        [else (key-handler (key-press key-code))]))
    (super-new)))

(define (space? key)
  (equal? key #\space))

(define (release? key)
  (equal? key 'release))

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
           [key-handler send-ground-message]))
    (send frame show #t)
    #;(define-values (x-max y-max) (send canvas get-client-size))
    #;(set-box! bot-right (posn x-max y-max))
    (define dc (send canvas get-dc))
    (spawn-renderer dc)))

(define PLAYER0 (rect (posn 0 0) 8 32))
(define GOAL0 (goal (rect (posn 300 200) 10 10)))

(define FRAMES-PER-SEC 24)

(define GRAVITY-PER-SEC 4)
(define JUMP-V -3)

(define MAX-V 8)

(define DX-PER-SEC 50)

(make-frame 1000 1000)
(spawn-timer-driver)
(spawn-player)
(spawn-horizontal-motion (/ (* 1.0 DX-PER-SEC) FRAMES-PER-SEC))
(spawn-vertical-motion (/ (* 1.0 GRAVITY-PER-SEC) FRAMES-PER-SEC) JUMP-V MAX-V)
(spawn-clock (/ 1000 FRAMES-PER-SEC))
(spawn-game-logic PLAYER0 GOAL0)
(spawn
 (lambda (e s) #f)
 (void)
 (assert (static (rect (posn 0 100) 175 10)))
 (assert (static (rect (posn 200 100) 200 10)))
 (assert (static (rect (posn 100 50) 10 50)))
 (assert (static (rect (posn 150 50) 50 10))))

